use std::collections::HashMap;
use std::path::PathBuf;
use std::fs;
use std::ops::Index;
use std::ops::Add;

use crate::built_in_functions::call_built_in_function;
use crate::built_in_functions::BUILT_IN_FUNCTIONS;
use crate::debug::EvaluatioError;
use crate::state;
use crate::tokenizer::Tokenizer;

// Define Class, Instance, and Value types for the evaluator
#[derive(Debug, Clone)]
pub struct Class {
    pub functions: HashMap<String, HashMap<String, Value>>,
    variables: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: String,
    variables: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub enum Value{
    Number(f64),
    List(Vec<Value>),
    Bool(bool),
    Str(String),
    Path(PathBuf),
    Instance(Instance),
    None,
}

impl Index<usize> for Value {
    type Output = Value;

    fn index(&self, idx: usize) -> &Self::Output {
        match self {
            Value::List(vec) => &vec[idx],
            _ => panic!("Indexing only supported on Value::List"),
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
            (Value::Str(a), Value::Str(b)) => Value::Str(a + &b),
            (Value::Str(a), Value::Number(b)) => Value::Str(a + &b.to_string()),
            (Value::Number(a), Value::Str(b)) => Value::Str(a.to_string() + &b),
            _ => Value::None,
        }
    }
}

// Define methods for Value type conversions and utilities
impl Value {
    pub fn as_f64(&self) -> f64 {
        match self {
            Value::Number(n) => n.clone(),
            Value::Bool(b) => if *b { 1.0 } else { 0.0 },
            Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
            _ => 0.0,
        }
    }
    pub fn as_usize(&self) -> usize {
        match self {
            Value::Number(n) => *n as usize,
            Value::Bool(b) => if *b { 1 } else { 0 }
            _ => 0,
        }
    }
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Number(n) => *n != 0.0,
            Value::Str(s) => !s.is_empty(),
            Value::None => false,
            _ => true,
        }
    }
    pub fn to_string_value(&self) -> String {
        match self {
            Value::Number(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Str(s) => s.clone(),
            Value::Path(p) => p.to_str().unwrap_or("").to_string(),
            Value::None => "None".into(),
            _ => "".to_string(),
            }
        }
    pub fn length(&self) -> usize {
        match self {
            Value::List(v) => v.len(),
            Value::Str(s) => s.len(),
            _ => 0,
        }
    }
    pub fn iter(&self) -> Box<dyn Iterator<Item = &Value> + '_> {
        match self {
            Value::List(v) => Box::new(v.iter()),
            _ => Box::new(std::iter::empty()),
        }
    }
    pub fn get_instance(&self) -> Option<Instance> {
        match self {
            Value::Instance(inst) => Some(inst.clone()),
            _ => None,
        }
    }
    pub fn is_none_value(&self) -> bool {
        match self {
            Value::None => true,
            _ => false,
        }
    }
}

// Define the Evaluator struct and its methods for evaluating IPL code
pub struct Evaluator{
    lines: Vec<String>,
    pub variables: HashMap<String, Value>,
    pub functions: HashMap<String, HashMap<String, Value>>,
    pub classes: HashMap<String, Class>,
    evaluators: HashMap<String, Evaluator>,
    indentation_stack : Vec<(String, usize)>,
    
    folder: String,
    path: PathBuf,
}

fn get_indentation(line: &str) -> usize {
    line.chars().take_while(|c| c.is_whitespace()).count()
}

impl Evaluator{
    pub fn new() -> Self{
        Self{
            lines: vec![],
            variables: HashMap::from([
                ("True".to_string(), Value::Bool(true)),
                ("False".to_string(), Value::Bool(false)),
                ("None".to_string(), Value::None),
            ]),
            functions: HashMap::new(),
            evaluators: HashMap::new(),
            classes: HashMap::new(),
            indentation_stack: vec![],

            folder: String::new(),
            path: PathBuf::new(),
        }
    }

    // Evaluate a file by reading its contents and executing its lines
    pub fn ev_file(&mut self, file: &str) {
        let path: PathBuf = PathBuf::from(file); // Convert file string to PathBuf
        self.path = path.clone();
        self.folder = path
            .parent()
            .and_then(|p| p.to_str())
            .unwrap_or("")
            .to_string();
        self.folder += "//"; // Get the folder path for imports
        let contents = fs::read_to_string(file).expect("Could not read file"); // Read file contents

        self.lines = contents
            .lines()
            .map(|line| line.to_string())
            .filter(|line| !line.trim().is_empty())
            .collect();

        self.lines.push("End of file".to_string()); // Add end marker to lines

        self.indentation_stack = vec![("normal".to_string(), 0)]; // Initialize indentation stack
        
        self.execute_lines(0, self.lines.len(), "".to_string()); // Execute the file
        
        println!("variables {:#?}", self.variables); 
        println!("classes {:#?}", self.classes);
        println!("functions {:#?}", self.functions);
    }

    // Evaluate a function by name with given arguments
    fn ev_func(&mut self, function_name: &str, args: Vec<Value>) -> Value {
        let file: &Value = &self.functions[function_name]["file"];
        if file.to_string_value() != self.path.to_str().unwrap() {
            if let Some(ev) = self.evaluators.get_mut(&file.to_string_value()) { // Check if evaluator for the file already exists
                return ev.ev_func(function_name, args);
            } else {
                EvaluatioError::new("Evaluator for file not found".to_string()).raise();
            }
        }

        let function_arguments: &Value = &self.functions[function_name]["arguments"].clone(); // Get function arguments
        let function_lines: &Value = &self.functions[function_name]["function_body"]; // Get function body lines

        // println!("Executing function {} with lines: {:?}", function_name, function_lines);
        // println!("Function lines content:");
        // for i in function_lines.iter() {
        //     println!("  {:?}: '{}'", i, self.lines[i.as_usize()]);
        // }
        if args.len() != function_arguments.length() { // Check argument count
            EvaluatioError::new("Wrong amount of arguments".to_string()).raise();
        }

        let global_vars: HashMap<String, Value> = self.variables.clone(); // Save current variables

        for (name, value) in function_arguments.iter().zip(args.iter()) {
            self.variables.insert(name.to_string_value(), value.clone()); // Set function arguments in variables
        }
        self.indentation_stack.push(("function".to_string(), get_indentation(&self.lines[function_lines[0].as_usize()]))); // Push function context to indentation stack

        // Execute function lines and get result
        let result: Value = self.execute_lines(function_lines[0].as_usize(), (function_lines[function_lines.length() - 1].clone() + Value::Number(1.0)).as_usize(), "".to_string()).clone();
    

        for name in function_arguments.iter(){
            if global_vars.contains_key(&name.to_string_value()){
                self.variables.insert(name.to_string_value(), global_vars.get(&name.to_string_value()).expect("The if for function argument ressetting failed").clone());
            }
            else{
                self.variables.remove(&name.to_string_value());
            }
        }
        self.indentation_stack.pop(); // Pop function context from indentation stack

        return result; // Return function result

    }

    // Evaluate a class method by instance string, method name, and arguments
    fn ev_class_func(&mut self, instance_str: String, function_name: &str, args: Vec<Value>, instance_opt: Option<Instance>, class_opt: Option<Class>) -> Value {
        // println!("ev_class_func called with instance: {}, function: {}, args: {:?}, instance_opt:{:?}", instance_str, function_name, args, instance_opt);
        let mut instance: Instance = Instance {
            class: "".to_string(),
            variables: HashMap::new(),
        };
        if instance_opt.is_some(){
            instance = instance_opt.unwrap();
            self.variables.insert(instance_str.clone(), Value::Instance(instance.clone()));
        } else {
            if !self.variables.contains_key(&instance_str){
                EvaluatioError::new("Instance not found".to_string()).raise();
            }
            else {
                instance = self.variables.get(&instance_str).expect("Instance not found").get_instance().expect("Not an instance");
            }
        }
        let mut class = Class {
            functions: HashMap::new(),
            variables: HashMap::new(),
        };
        if class_opt.is_some(){
            class = class_opt.unwrap();
            self.classes.insert(instance.class.clone(), class.clone());
        } else {
            if !self.classes.contains_key(&instance.class){
                EvaluatioError::new("Class not found".to_string()).raise();
            }
            else {
                class = self.classes.get(&instance.class).expect("Class not found").clone();
            }
        }
        if !class.functions.contains_key(function_name){
            return Value::None;
        }
        // println!("Self.classes: {:#?}", self.classes);
        let file = class.functions[function_name]["file"].clone();
        if file.to_string_value() != self.path.to_str().unwrap() {
            if let Some(ev) = self.evaluators.get_mut(&file.to_string_value()) {
                return ev.ev_class_func(instance_str, function_name, args, Some(instance), Some(class));
            }   else {
                EvaluatioError::new("Evaluator for file not found".to_string()).raise();
                }
        }

        let function_arguments: &Value = &class.functions[function_name]["arguments"];
        let function_lines: &Value = &class.functions[function_name]["function_body"];

        // println!("Executing class function {} with lines: {:?}", function_name, function_lines);
        // println!("Function lines content:");
        // for i in function_lines.iter() {
        //     println!("  {:?}: '{}'", i, self.lines[i.as_usize()]);
        // }
        if args.len() != function_arguments.length() {
            EvaluatioError::new("Wrong amount of arguments".to_string()).raise();
        }
        let global_vars = self.variables.clone();

        // println!("function_arguments: {:?} and args: {:?}", function_arguments, args);
        for (name, value) in function_arguments.iter().zip(args.iter()) {
            // println!("Setting variable {} to {:?}", name.to_string_value(), value);
            self.variables.insert(name.to_string_value(), value.clone());
        }
        // println!("self.variables before function execution: {:#?}", self.variables);
        // println!("self.classes before function execution: {:#?}", self.classes);
        self.indentation_stack.push(("function".to_string(), get_indentation(&self.lines[function_lines[0].as_usize()])));

        
        let result = self.execute_lines(function_lines[0].as_usize(), (function_lines[function_lines.length() - 1].clone() + Value::Number(1.0)).as_usize(), instance_str.clone());
        // println!("self.variables after function execution: {:#?}", self.variables);
        self.indentation_stack.pop();

        for name in function_arguments.iter(){
            if global_vars.contains_key(&name.to_string_value()){
                self.variables.insert(name.to_string_value(), global_vars.get(&name.to_string_value()).expect("The if for function argument ressetting failed").clone());
            }
            else{
                self.variables.remove(&name.to_string_value());
            }
        }

        return result;
    }

    fn execute_lines(&mut self, start: usize, end: usize, self_value: String) -> Value {
        let mut programm_counter: usize = start;

        println!("execute_lines called with start {} and end {}", start, end);

        while programm_counter < end{
            // println!("At line {}", programm_counter);

            let mut line = self.lines[programm_counter].clone();
            line = line.split("#").collect::<Vec<_>>()[0].to_string();

            // println!("Current line: '{}'", line);

            state::set_programm_state(programm_counter, &line);

            let indentation = get_indentation(&line);

            line = line.trim().to_string();

            // println!("Indentation_stack: {:?}", self.indentation_stack);
            if indentation <= self.indentation_stack.last().unwrap_or(&("".to_string(), 0)).1{
                if self.indentation_stack.last().is_none(){
                    return Value::None;
                }
                if self.indentation_stack[self.indentation_stack.len() - 1].0 == "while"{
                    while self.lines[programm_counter].split(" ").collect::<Vec<_>>()[0] != "while"{
                        programm_counter -= 1;
                    }
                    self.indentation_stack.pop();
                    continue
                }
            } else if self.indentation_stack[self.indentation_stack.len() - 1].0 == "if"{
                self.indentation_stack.pop();
            } else if self.indentation_stack[self.indentation_stack.len() - 1].0 == "else"{
                self.indentation_stack.pop();
            }

            match line.split(" ").collect::<Vec<_>>()[0]{ //TODO: also check here, if split_once() is the better option
                "import" => {
                    let file = self.folder.clone() + line.split(" ").collect::<Vec<_>>()[1];
                    self.evaluators.insert(file.clone(), Evaluator::new());
                    if let Some(evaluator) = self.evaluators.get_mut(&file) {
                        evaluator.ev_file(&file);
                    }
                    self.functions.extend(self.evaluators[&file].functions.clone());
                    self.variables.extend(self.evaluators[&file].variables.clone());
                    self.classes.extend(self.evaluators[&file].classes.clone());

                    programm_counter += 1;
                    }
                "while" => {
                    let mut result = false;
                    if let Some((_first, rest)) = line.split_once(' ') {
                        result = self.ev_expr(rest).as_bool();
                    }
                    if result == true{
                        programm_counter += 1;
                        self.indentation_stack.push(("while".to_string(), indentation));
                    } else{
                        programm_counter += 1;
                        while get_indentation(&self.lines[programm_counter].clone()) > self.indentation_stack[self.indentation_stack.len() - 1].1{
                            programm_counter += 1
                        }
                    }
                }
                "for" =>{
                    let variable_name = line.split(" ").collect::<Vec<_>>()[1];
                    let iterable_expr = line.split("in").collect::<Vec<_>>()[1].trim();
                    // println!("For loop variable: {}, iterable expression: {}", variable_name, iterable_expr);
                    let iterable = self.ev_expr(iterable_expr); 
                    // println!("Iterable evaluated to: {:?}", iterable);
                    let start_line = programm_counter + 1;
                    let mut end_line = start_line;
                    while get_indentation(&self.lines[end_line]) > indentation{
                        end_line += 1;
                    }

                    self.indentation_stack.push(("for".to_string(), indentation));
                    for value in iterable.iter(){
                        self.variables.insert(variable_name.to_string(), value.clone());
                        self.execute_lines(start_line, end_line, "".to_string());
                    }
                    self.indentation_stack.pop();
                    programm_counter = end_line;
                }
                "if" => {
                    let mut result = false;
                    if let Some((_first, rest)) = line.split_once(' ') {
                        result = self.ev_expr(rest).as_bool();
                    }
                    if result == true{
                        programm_counter += 1;
                        self.indentation_stack.push(("if".to_string(), indentation));
                    }
                    else{
                        programm_counter += 1;
                        while programm_counter < end{
                            let current_line = self.lines[programm_counter].clone();
                            let current_indent = get_indentation(&current_line);
                            let first_word = current_line.split_whitespace().next().unwrap_or("");
                            
                            if current_indent > indentation{
                                programm_counter += 1;
                                continue;
                            }
                            if first_word == "elif" {
                                if let Some((_first, rest)) = current_line.split_once(' ') {
                                    if self.ev_expr(rest).as_bool() {
                                        programm_counter += 1;
                                        self.indentation_stack.push(("if".to_string(), indentation));
                                        break;
                                    } else {
                                        // Skip block
                                        programm_counter += 1;
                                        while programm_counter < end && get_indentation(&self.lines[programm_counter]) > indentation {
                                            programm_counter += 1;
                                        }
                                    }
                                }
                            } else if first_word == "else" {
                                programm_counter += 1;
                                self.indentation_stack.push(("else".to_string(), indentation));
                                break;
                            } else if current_indent <= indentation{
                                break;
                            } else {
                                programm_counter += 1;
                                break;
                            } 
                        }
                    }
                }
                "else" => {
                    programm_counter += 1;
                    while get_indentation(&self.lines[programm_counter].clone()) > self.indentation_stack[self.indentation_stack.len() - 1].1{
                        programm_counter += 1;
                    }
                }
                "elif" => {
                    programm_counter += 1;
                    while get_indentation(&self.lines[programm_counter].clone()) > self.indentation_stack[self.indentation_stack.len() - 1].1{
                        programm_counter += 1;
                    }
                }
                "break" => {
                    while let Some(x) = self.indentation_stack.pop(){
                        if x.0 == "while"{
                            while get_indentation(&self.lines[programm_counter]) > x.1{
                                programm_counter += 1;
                            }
                            break;
                       }
                        else if x.0 == "normal"{
                            EvaluatioError::new("Error: 'break' outside loop".to_string()).raise();
                        }
                    }
                } 
                "continue" => {
                    while let Some(x) = self.indentation_stack.pop(){
                        if x.0 == "while"{
                            while self.lines[programm_counter].split(" ").collect::<Vec<_>>()[0] != "while"{
                                programm_counter -= 1;
                                continue;
                            }
                        }
                        else if x.0 == "for" {
                            return Value::None; // Stop execution of current iteration
                        }
                        else if x.0 == "normal"{
                            EvaluatioError::new("Error: 'continue' outside loop".to_string()).raise();
                        }
                    }
                } 
                "return" => {
                    let expr: &str = line.split("return").collect::<Vec<_>>()[1];
                    return self.ev_expr(expr);
                }
                "class" => {
                    let class_name: &str = line.split(" ").collect::<Vec<_>>()[1].trim();
                    let base_class: &str = if line.contains(":"){
                        line.split(":").collect::<Vec<_>>()[1].trim()
                    } else{
                        ""
                    };
                    let start_line = programm_counter + 1;
                    let mut end_line = start_line;
                    while get_indentation(&self.lines[end_line]) > indentation{
                        end_line += 1;
                    }
                    
                    self.indentation_stack.push(("class".to_string(), indentation + 1));
                    let funcs = self.functions.clone();
                    self.functions.clear();

                    self.classes.insert(class_name.to_string(), Class {
                        functions: HashMap::new(),
                        variables: HashMap::new(),
                    });

                    if base_class != ""{
                        self.classes.get_mut(class_name).unwrap().variables = self.classes[base_class].variables.clone();
                        self.classes.get_mut(class_name).unwrap().functions = self.classes[base_class].functions.clone();
                    }

                    self.execute_lines(start_line, end_line, class_name.to_string());

                    self.classes.get_mut(class_name).unwrap().functions.extend(self.functions.clone());

                    self.functions = funcs;

                    self.indentation_stack.pop();
                    programm_counter = end_line;
                }
                "def" => {
                    let function_decleration = match line.split_once(' ') {
                        Some((_, declaration)) => declaration.trim(),
                        None => "",
                    };
                    if function_decleration == ""{
                        EvaluatioError::new("def requires a function decleration".to_string()).raise();
                    }
                    let function_name = function_decleration.split("(").collect::<Vec<_>>()[0];
                    let args = function_decleration
                                        .split_once('(') // returns Option<(&str, &str)>
                                        .and_then(|(_, rest)| rest.split_once(')')) // safely get the inside of the parentheses
                        .map(|(args_str, _)| {
                            args_str
                                .split(',')
                                .map(str::trim)
                                .filter(|s| !s.is_empty())
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_else(|| Vec::new());
                    let function_arguments = args.iter().map(|n| Value::Str(n.to_string())).collect::<Vec<Value>>();
                    programm_counter += 1;
                    let start_line = programm_counter;
                    while get_indentation(&self.lines[programm_counter]) > indentation{
                        programm_counter += 1;
                    }
                    let function_lines = (start_line..programm_counter)
                        .map(|n| Value::Number(n as f64))
                        .collect::<Vec<Value>>();
                    let mut function_hash_map: HashMap<String, Value> = HashMap::new();
                    function_hash_map.insert("file".to_string(), Value::Path(self.path.clone()));
                    function_hash_map.insert("arguments".to_string(), Value::List(function_arguments));
                    function_hash_map.insert("function_body".to_string(), Value::List(function_lines));
                    // println!("Function line {} : {:?}", function_decleration, function_hash_map);
                    self.functions.insert(function_name.to_string(), function_hash_map);
                }
                _ => {
                    if line == "End of file"{
                        break;
                    }
                    if line.contains("="){
                        if let Some((mut variable_name, expr)) = line.split_once("="){
                            let result = self.ev_expr(expr);

                            variable_name = variable_name.trim();
                            if variable_name.contains("self"){
                                if self_value.is_empty(){
                                    EvaluatioError::new("self used outside class".to_string()).raise();
                                }
                                else{
                                    let var_name = variable_name.split(".").collect::<Vec<_>>()[1];
                                    if self.classes.contains_key(&self_value){
                                        self.classes.get_mut(&self_value).unwrap().variables.insert(var_name.to_string(), result);
                                    }
                                    else if self.variables.contains_key(&self_value) {
                                        let inst_var = self.variables.get(&self_value);
                                        if inst_var.is_none(){
                                            EvaluatioError::new("Self used outsside of class".to_string()).raise();
                                        }

                                        let inst_opt = inst_var.unwrap().get_instance();
                                        if inst_opt.is_none(){
                                            EvaluatioError::new("Self unwrapping returned a null value".to_string()).raise();
                                        }
                                        let mut inst = inst_opt.unwrap();

                                        inst.variables.insert(var_name.to_string(), result);
                                        self.variables.insert(self_value.clone(), Value::Instance(inst));

                                        //let inst = self.variables.get_mut(&self_value).unwrap_or(&mut Value::None).get_instance().unwrap_or(Instance {class: "".to_string(), variables: HashMap::new()}).variables.insert(var_name.to_string(), result);
                                        //self.variables.insert(self_value.clone(), inst.unwrap_or(Value::None));
                                    }
                                    else {
                                        EvaluatioError::new("Self reference to class or instance not found".to_string()).raise();
                                    }
                                }
                                
                            }
                            else if variable_name.contains("."){
                                let object = variable_name.split(".").collect::<Vec<_>>()[0];
                                let var_name = variable_name.split(".").collect::<Vec<_>>()[1];
                                if self.variables.contains_key(object){
                                    let mut inst = self.variables.get_mut(object).unwrap().get_instance().expect("Not an instance");
                                    inst.variables.insert(var_name.to_string(), result);

                                    self.variables.insert(object.to_string(), Value::Instance(inst));
                                }
                                else if self.classes.contains_key(object){
                                    self.classes.get_mut(object).unwrap().variables.insert(var_name.to_string(), result);
                                }
                                else{
                                    EvaluatioError::new("Class not found".to_string()).raise();
                                }
                                
                            }
                            else{
                                self.variables.insert(variable_name.to_string(), result);
                            }
                            // println!("Variable name: {}, expr: {}", variable_name, expr);
                        }
                    }
                    else{
                        self.ev_expr(&line);
                    }
                    programm_counter += 1;
                }
            }
        }
        return Value::None;
    }

    fn ev_expr(&mut self, expr: &str) -> Value {
        let tokens = Tokenizer::new().tokenize(expr, self.variables.clone(), self.functions.clone(), self.classes.clone());

        // println!("tokens: {:?}", tokens);

        let mut stack: Vec<Value> = vec![];
        let mut i = 0;
        while i < tokens.len(){
            let token = tokens.get(i).expect("Empty token");
            let token_str = token.to_string_value();
            // println!("token: {} , stack: {:?}", token_str, stack);
            if token_str.trim_matches('.').parse::<f64>().is_ok(){
                stack.push(Value::Number(token_str.parse::<f64>().unwrap()));
            }
            else if (token_str.starts_with('"') && token_str.ends_with('"')) || (token_str.starts_with("'") && token_str.ends_with("'")){
                stack.push(Value::Str(token_str[1..token_str.len()-1].to_string()));
            }
            else if matches!(token, Value::List(_)) {
                stack.push(token.clone());
            }
            else if self.variables.contains_key(&token_str) {
                stack.push(self.variables[&token_str].clone());
            }
            else if self.functions.contains_key(&token_str)|| BUILT_IN_FUNCTIONS.contains_key(&token_str as &str) || self.classes.contains_key(&token_str) {
                let function_name = &token_str;
                let mut args: Vec<Value> = vec![];
                // println!("Function call detected: {}", function_name);
                // println!("Functions: {:?}", self.functions);
                let function_args = tokens.get(i + 1);
                for arg in function_args.unwrap_or(&Value::None).iter(){
                    if let Value::Str(s) = arg {
                        let evaluated_arg = self.ev_expr(s);
                        args.push(evaluated_arg);
                    } else {
                        args.push(arg.clone());
                    }
                }
                if args.len() == 1 && args[0].to_string_value() == Value::None.to_string_value(){
                    args = vec![];
                }
                // println!("Function {} called with arguments: {:?}", function_name, args);
                let result = if BUILT_IN_FUNCTIONS.contains_key(function_name as &str) {
                    call_built_in_function(function_name, args)
                } else if self.functions.contains_key(function_name) {
                    self.ev_func(function_name, args)
                } else if self.classes.contains_key(function_name) {
                    let instance = Instance {
                        class: function_name.to_string(),
                        variables: self.classes[function_name].variables.clone(),
                    };
                    self.ev_class_func("__DO_NOT_USE_THIS_VARIABLE_INTERNAL_ONLY__".trim().to_string(), &function_name, args, Some(instance.clone()), None);
                    
                    Value::Instance(self.variables.get("__DO_NOT_USE_THIS_VARIABLE_INTERNAL_ONLY__").expect("Instance not found").get_instance().expect("Not an instance"))
                } else {
                    Value::None
                };
                stack.push(result);
                i += 1; // Skip the next token which is the argument list
            }
            else if token.to_string_value() == "."{
                let instance = stack.pop().expect("No instance before .");
                let attribute = tokens.get(i+1).expect("No attribute after .");
                match instance {
                    Value::Instance(inst) => {
                        if inst.variables.contains_key(&attribute.to_string_value()){
                            stack.push(inst.variables[&attribute.to_string_value()].clone());
                        }
                        else if self.classes.contains_key(&inst.class){
                            let class = &self.classes[&inst.class];
                            if class.functions.contains_key(&attribute.to_string_value()){
                                let function_name = &attribute.to_string_value();
                                let mut args: Vec<Value> = vec![];
                                let function_args = tokens.get(i + 2);
                                for arg in function_args.unwrap_or(&Value::None).iter(){
                                    if let Value::Str(s) = arg {
                                        let evaluated_arg = self.ev_expr(s);
                                        args.push(evaluated_arg);
                                    } else {
                                        args.push(arg.clone());
                                    }
                                }
                                if args.len() == 1 && args[0].to_string_value() == Value::None.to_string_value(){
                                    args = vec![];
                                }
                                // println!("Class function {} called with arguments: {:?}", function_name, args);
                                let result = self.ev_class_func(tokens[i - 1].to_string_value(), function_name, args, None, None);
                                stack.push(result);
                                i += 1; // Skip the next token which is the argument list
                            }
                            
                        }
                        else{
                            EvaluatioError::new(format!("Instance has no attribute {}", attribute.to_string_value())).raise();
                        }
                    }
                    _ => EvaluatioError::new("Left side of '.' is not an instance".to_string()).raise(),
                }
                i += 1; // Skip the attribute token
            }
            else{ 

                let rhs = stack.pop().expect("Not enough values on stack");
                let lhs = stack.pop().expect("Not enough values on stack");
                

                if token_str == "+"{
                    stack.push(Value::Number(lhs.as_f64() + rhs.as_f64()));
                }
                else if token_str == "-" {
                    stack.push(Value::Number(lhs.as_f64() - rhs.as_f64()));
                }
                else if token_str == "*" {
                    stack.push(Value::Number(lhs.as_f64() * rhs.as_f64()));
                }
                else if token_str == "/" {
                    stack.push(Value::Number(lhs.as_f64() / rhs.as_f64()));
                }

                else if token_str == "==" {
                    stack.push(Value::Bool(lhs.as_f64() == rhs.as_f64()));
                }
                else if token_str == "!=" {
                    stack.push(Value::Bool(lhs.as_f64() != rhs.as_f64()));
                }
                else if token_str == "<" {
                    stack.push(Value::Bool(lhs.as_f64() < rhs.as_f64()));
                }
                else if token_str == "<=" {
                    stack.push(Value::Bool(lhs.as_f64() <= rhs.as_f64()));
                }
                else if token_str == ">" {
                    stack.push(Value::Bool(lhs.as_f64() > rhs.as_f64()));
                }
                else if token_str == ">=" {
                    stack.push(Value::Bool(lhs.as_f64() >= rhs.as_f64()));
                }

                else if token_str == "and" {
                    stack.push(Value::Bool(lhs.as_bool() && rhs.as_bool()));    
                }
                else if token_str == "or" {
                    stack.push(Value::Bool(lhs.as_bool() || rhs.as_bool()));
                }
            }
            i += 1;
        }
    return stack.pop().unwrap_or(Value::None); 
    }

}
