import java.util.Vector;

public class SemanticAnalyzer implements ASTVisitor {

    private VariableEnvironment variableEnv;
    private FunctionEnvironment functionEnv;
    private TypeEnvironment typeEnv;
    
    private boolean addFormalsToVarEnv;

    public SemanticAnalyzer() {
        variableEnv = new VariableEnvironment();
        functionEnv = new FunctionEnvironment();
        functionEnv.addBuiltinFunctions();
        typeEnv = new TypeEnvironment();
        addFormalsToVarEnv = false;
    }
    
    /**
     * Checks if size element of array is an integer. Gives error if not. If it is an integer, adds
     * type to type environment if it does not exist.
     * 
     * @param newarrayexpression
     * @return Integer instance if error, instance of ArrayType object if not
     */
    public Object VisitNewArrayExpression(ASTNewArrayExpression newarrayexpression) {
       Type size = (Type) newarrayexpression.elements().Accept(this);
               
        if (size != IntegerType.instance()) {
            CompError.message(newarrayexpression.line(), "Array size must be an integer.");
            return IntegerType.instance();
        } else {
            //type is base type (i.e. int for int[][])
            return insertArrayType(newarrayexpression.type(), newarrayexpression.arraydimension(), newarrayexpression.line());
        }        
    }   /* DONE */
    
    /**
     * Adds all brackets to base type for array type. 
     * 
     * If entry does not exist in Type Environment,
     * recursively calls itself with an array dimension - 1. This ensures that all array types with
     * same base but less array dimension will be added if they are missing. It then creates a new
     * ArrayType instance by passing in the Type instance in from the ArrayType with one less array
     * dimensionality into the ArrayType constructor. This new ArrayType instance is inserted into
     * the Type Environment with the key <basetype + ("[]" * arraydimension)>, and is then returned.
     * 
     * However, if the entry exists, it is returned. 
     * 
     * @param type - base type of array
     * @param arraydimension
     * @param linenum
     * @return ArrayType entry for the specific array with base type "type" and the provided array
     * dimensionality.
     */
    public Type insertArrayType(String type, int arraydimension, int linenum) {
        System.out.println("I am hitting insertArrayType.");
        if (typeEnv.find(type) == null) {
            CompError.message(linenum, "Base Type does not exist.");
            return IntegerType.instance();
        }
        String thisType = type;
        for (int i = 0; i < arraydimension; i++) {
            thisType += "[]";
        }
        //I see potential problems here
        ArrayType entry = new ArrayType(typeEnv.find(thisType));



        //if type is not in type environment, recursive call to add it
        //will add previous dimensional ones if not in environment either
        if (entry == null) {
            ArrayType prevDimTypeEntry = (ArrayType) insertArrayType(type, arraydimension - 1, linenum);
            ArrayType arrTypeInstance = new ArrayType(prevDimTypeEntry);
            typeEnv.insert(thisType, arrTypeInstance);
            return arrTypeInstance;
        } else {
            return entry;
        }
    }   /* DONE */
    
    /**
     * Visits each class.
     * 
     * @param classes
     */
    public Object VisitClasses(ASTClasses classes){
        for (int i = 0; i <classes.size(); i++) {
            classes.elementAt(i).Accept(this);
        }
        //TODO: do we need to start a new function environment for each class?
        return null;
    }   /* DONE */
    
    /**
     * Checks that types of left-hand side and right-hand side of the Assignement
     * statement are the same.
     * 
     * @param assignstatement
     */
    public Object VisitAssignmentStatement(ASTAssignmentStatement assignstatement) {
        Type lhs = (Type) assignstatement.variable().Accept(this);
        Type rhs = (Type) assignstatement.value().Accept(this);
        
        if (lhs != rhs) {
            CompError.message(assignstatement.line(), "Lefthand side and righthand "
                    + "side of and assignement statement must match.");
        }
        return null;
    }   /* DONE */
    
    /**
     * Checks base variable and checks if index is an integer.
     * 
     * @param arrayvariable
     */
    public Object VisitArrayVariable(ASTArrayVariable arrayvariable) {
        arrayvariable.base().Accept(this);
        Type typeOfIndex = (Type) arrayvariable.index().Accept(this);
        
        if (typeOfIndex != IntegerType.instance()) {
            CompError.message(arrayvariable.line(), "Index of an array must by of "
                    + "type integer.");
        }
        return null;
    }   /* DONE */
    
    /**
     * Returns the instance of BooleanType.
     * 
     * @param booliteral
     * @return BooleanType's instance
     */
    public Object VisitBooleanLiteral(ASTBooleanLiteral boolliteral) {
        return BooleanType.instance();
    }   /* DONE */
    
    /**
     * Creates a new variable environment for the class type, goes through each
     * variable definition and inserts it into class's variable environment, then
     * creates new ClassType and inserts it into global type environment.
     * 
     * @param asclass
     * @return IntegerType instance if error, else ClassType
     */
    public Object VisitClass(ASTClass asclass) {
        if (typeEnv.find(asclass.name()) != null) {
            CompError.message(asclass.line(), "Cannot have classes with the same name.");
            return IntegerType.instance();
        }
        
        VariableEnvironment variables = new VariableEnvironment();
        
        ASTInstanceVariableDefs variabledefs = asclass.variabledefs();
        Type type;
        if (variabledefs != null) {
            variabledefs.Accept(this);
            //Go through each variable definition and insert it into class's 
            //variable environment
            ASTInstanceVariableDef vardef;
            for (int i = 0; i < variabledefs.size(); i++) {
                vardef = variabledefs.elementAt(i);
                type = typeEnv.find(vardef.type());
                //If there is a variable def of same name already in the class's variable enviro,
                //give an error
                if (variables.find(vardef.name()) != null) {
                    CompError.message(vardef.line(), "Cannot have 2 instance variables"
                            + "of the same name within the same class.");
                    return IntegerType.instance();
                }
                variables.insert(vardef.name(), new VariableEntry(type));
            }
        }
        ClassType classType = new ClassType(variables);
        //Create new Type entry for class
        typeEnv.insert(asclass.name(), classType);
        return classType;
    }   /* DONE */
    
    /**
     * Checks to make sure type of definition is in typeEnv. if not, error
     * 
     * @param variabledef
     * @return IntegerType instance if type is n
     */
    public Object VisitInstanceVariableDef(ASTInstanceVariableDef variabledef)
    {
        //add the variable here to var environment since insertArrayType does not handle it
        //variableEnv.insert(formal.name(), new VariableEntry(type));
        //array[i][j], if array[i], no longer the same variable so no need to keep track of it
        Object returnobject;
        ////
        System.out.println("IN Instance VariableDef");
        returnobject = insertArrayType(variabledef.type(), variabledef.arraydimension(), variabledef.line());
        Type instancevariableType = typeEnv.find(variabledef.type());
        variableEnv.insert(variabledef.name(), new VariableEntry(instancevariableType));
        System.out.println("NAME " + variabledef.name());

        return returnobject;
        //TODO: should we even be returning anything?
        //this may apply to all but we need to store the bracket count to be
        //retrieved later from the parse table
        //what do we store here
    }   /* DONE */
    
    /**
     * Calls accept on base variable.
     * 
     * @param classvar
     */
    public Object VisitClassVariable(ASTClassVariable classvar){
        classvar.base().Accept(this);
        return null;
    }   /* DONE */
    
    /**
     * Begins new scope on the variable environment, calls Accept() of 
     * the iterator initialization statement, the test, the increment 
     * statement, and the body, then ends scope.
     * 
     * @param forstatement
     */
    public Object VisitForStatement(ASTForStatement forstatement) {
        variableEnv.beginScope();
        forstatement.initialize().Accept(this);
        forstatement.test().Accept(this);
        forstatement.increment().Accept(this);
        forstatement.body().Accept(this);
        variableEnv.endScope();
        return null;
    }   /* DONE */
    
    /**
     * Returns null. There is nothing to be done for an empty statement.
     * 
     * @param emptystate
     */
    public Object VisitEmptyStatement(ASTEmptyStatement emptystate) {

        return null;
    }   /* DONE */
        
    /**
     * Begins new scope on the variable environment, calls Accept() of 
     * the test and the body, then ends scope.
     * 
     * @param dowhile
     */
    public Object VisitDoWhileStatement(ASTDoWhileStatement dowhile) {
        variableEnv.beginScope();
        dowhile.test().Accept(this);
        dowhile.body().Accept(this);
        variableEnv.endScope();
        return null;
    }   /* DONE */
    
    /**
     * Checks that formal is correct by calling helper method insertArrayType()
     * 
     * @param formal
     * @return Type of formal. If Integer, may be error.
     */
    public Object VisitFormal(ASTFormal formal) {
        Type type = insertArrayType(formal.type(), formal.arraydimension(), formal.line());
               
        //Add formals to variable environment if option is set to true
        if (addFormalsToVarEnv) {
            if (variableEnv.find(formal.name()) != null) {
                CompError.message(formal.line(), "Cannot have 2 formals"
                        + "of the same name for the same function.");
                return IntegerType.instance();
            }
            variableEnv.insert(formal.name(), new VariableEntry(type));
        }     
        return type;
    }   /* DONE */
    
    /**
     * Visits each formal.
     * 
     * @param formals
     */
    public Object VisitFormals(ASTFormals formals) {
        if (formals != null) {  /* also had formals.size() == 0 but removed it */
            for (int i=0; i<formals.size(); i++) {
                formals.elementAt(i).Accept(this);
            }
        }
        return null;
    }   /* DONE */
    
    /**
     * If prototype already exists, compares prototype and function
     * - compares number of formals
     * - compares types of each formal
     * - compares return types
     * Else,
     * - checks if return type exists 
     * - setup: sets hasPrototype to false so that function entry will be added later
     * 
     * Begins a new scope in variable environment.
     * Adds formals to variable environment: addFormalsToVarEnv = true
     * Adds function entry to function environment if no prototype.
     * Sets addFormalsToVarEnv to false.
     * Analyzes body of function.
     * Ends scope.
     * 
     * @param function
     */
    public Object VisitFunction(ASTFunction function) {
        boolean hasPrototype;
        //Analyze formal parameters & return type
        FunctionEntry funcEntry = functionEnv.find(function.name());
        //Check against prototype (if there is one), 
        if (funcEntry != null) {
            hasPrototype = true;
            Vector<Type> funcEntryFormals = funcEntry.formals();      //list of Type objects of function prototype formals
            ASTFormals functionFormals = function.formals();    //list of ASTFormal objects
            
            //- Is the return type the same?
            if (! funcEntry.result().equals(function.type())) {
                CompError.message(function.line(), "A function's return type must match"
                        + "with its function prototype's return type.");
                return null;
            }
            
            //- Check number of formals
            if (funcEntryFormals.size() != functionFormals.size()) {
                CompError.message(function.line(), "A function's formal parameters must match"
                        + "with its function prototype's formal parameters.");
                return null;
            }
            
            //- Are the formals the same? -- type
            for (int i = 0; i < funcEntryFormals.size(); i++) {
                Type functionFormalType = typeEnv.find(functionFormals.elementAt(i).type());
                //Check if type doesn't equal it's counterpart in its function prototype
                //-- don't have to check if type exists because prototype would have done this, just
                //   comparing types
                if (! (funcEntryFormals.elementAt(i).equals(functionFormalType))) {
                    CompError.message(function.line(), "A function's formal parameters must match"
                            + "with its function prototype's formal parameters.");
                    return null;
                }
            }
        } else {        //or add function entry to function environment (if no prototype)
            hasPrototype = false;
            //check if return type exists
            if (typeEnv.find(function.type()) == null) {
                CompError.message(function.line(), "The return type of the funciton is not a valid type.");
                return null;    //TODO: do we actually return null or integer instance?
            }
            //don't add function entry to function enviro yet... 
            //Add it after VisitFormal adds possible new dimension array types to typeEnv
        }
                
        //Begin a new scope in the variable environment
        variableEnv.beginScope();       
        
        //VisitFormal should add formals to variable environment in this case.
        addFormalsToVarEnv = true;

        Vector<Type> params = new Vector<Type>();
        if (function.formals() != null) {
            Type paramType;
            for (int i=0; i < function.formals().size(); i++) {
                //Add formal parameters to the variable environment  
                paramType = (Type) function.formals().elementAt(i).Accept(this);
                params.add(paramType);
            }
        }
        //Add function entry to function environment if there is no prototype
        if (!hasPrototype) {
            functionEnv.insert(function.name(), new FunctionEntry(typeEnv.find(function.type()), params));
        }
        addFormalsToVarEnv = false;
        Type typeofreturn = typeEnv.find(function.type());
        VariableEntry varentry = new VariableEntry(typeofreturn);
        //assumming that there will be no more than one "return" at a time
        variableEnv.insert("return", varentry);
        //Analyze the body of the function, using modified variable environment
        function.body().Accept(this);
        //End current scope in variable environment
        variableEnv.endScope();


        return null;    //TODO: or return type of function?
    }   /* DONE */
    
    public Object VisitFunctionCallExpression(ASTFunctionCallExpression callexpression) {
        //check to see if function exists in func environment
        FunctionEntry funcEntry = functionEnv.find(callexpression.name());
        if (funcEntry == null) {
            CompError.message(callexpression.line(), "Function " + callexpression.name() + " is not defined in this " +
                              "scope");
            return IntegerType.instance();
        }
        Type argType;
        for (int i=0; i<callexpression.size(); i++) {
            //check to see if the args used have the types they are supposed to have
            argType = (Type) callexpression.elementAt(i).Accept(this);
            if (funcEntry.formals().elementAt(i) != argType) {
                CompError.message(callexpression.line(), "Argument " + i + " for function " + callexpression.name() + " does not match"
                        + "its corresponding function parameter's type.");
                return IntegerType.instance();
            }
        }
        return funcEntry.result();
    }   /* DONE */
    
    public Object VisitFunctionCallStatement(ASTFunctionCallStatement statement) {
        //check to see if function exists in func env.
        FunctionEntry funcEntry = functionEnv.find(statement.name());
        if (funcEntry == null) {
            CompError.message(statement.line(), "Function " + statement.name() + " is not defined in this " +
                              "scope");
        }
        Type argType;
        for (int i=0; i<statement.size(); i++) {           
            //check to see if the args used have the types they are supposed to have
            argType = (Type) statement.elementAt(i).Accept(this);
            if (funcEntry.formals().elementAt(i) != argType) {
                CompError.message(statement.line(), "Argument " + i + " for function " + statement.name() + " does not match"
                        + "its corresponding function parameter's type.");
            }
        }
        return null;
    }   /* DONE */
    
    public Object VisitInstanceVariableDefs(ASTInstanceVariableDefs variabledefs) {
        System.out.print("I am hitting visitinstancevariabledefs.");
        for (int i=0; i<variabledefs.size(); i++) {
            //Check if type exists by calling VisitInstanceVariableDef() which calls insertArrayType(), which
            //checks the type and adds the according n-dimensional array types if the base type exists.
            //No need to add variable to environment because it is added into a local environment in VisitClass()
            variabledefs.elementAt(i).Accept(this);
        }
        return null;
    }   /* DONE */
    
    public Object VisitNewClassExpression(ASTNewClassExpression classexpression) {
        //check to see if the type is valid, in this case the type is a custom class type
        Type classType = typeEnv.find(classexpression.type());
        if (classType == null) {
            CompError.message(classexpression.line(), "Class type" + classexpression.type() + " is not defined in this " +
                    "scope");
            return IntegerType.instance();
        }

        return null;
    }   /* DONE */
    
    public Object VisitOperatorExpression(ASTOperatorExpression opexpression) {
        Type lhs = (Type) opexpression.left().Accept(this);
        Type rhs = (Type) opexpression.right().Accept(this);
        // NOTE: "Not" operator is taken care of by VisitUnaryOperatorExpression()
        
        switch (opexpression.operator()) {
            case ASTOperatorExpression.BAD_OPERATOR:    // Is this necessary?
                return IntegerType.instance();
                
            case ASTOperatorExpression.PLUS:
            case ASTOperatorExpression.MINUS:
            case ASTOperatorExpression.MULTIPLY:
            case ASTOperatorExpression.DIVIDE:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), "+,-,*,/ arithmetic binary "
                            + "operators require integer operands");
                }
                return IntegerType.instance();
                
            case ASTOperatorExpression.GREATER_THAN:
            case ASTOperatorExpression.GREATER_THAN_EQUAL:
            case ASTOperatorExpression.LESS_THAN:
            case ASTOperatorExpression.LESS_THAN_EQUAL:
            case ASTOperatorExpression.EQUAL:
            case ASTOperatorExpression.NOT_EQUAL:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), ">, >=, <, <=, != comparative "
                            + "binary operators require integer operands");
                    return IntegerType.instance();
                }
                return BooleanType.instance();
                
            case ASTOperatorExpression.AND:
            case ASTOperatorExpression.OR:
                if (lhs != BooleanType.instance() || rhs != BooleanType.instance()) {
                    CompError.message(opexpression.line(), "&&, || boolean binary operators "
                            + "require boolean operands");
                    return IntegerType.instance();
                }
                return BooleanType.instance();
        }
                
        return IntegerType.instance();
    }   /* DONE */
    
    public Object VisitFunctionDefinitions(ASTFunctionDefinitions fundefinitions) {
        //DONE (Added galles rec for return statement)
        for (int i=0; i < fundefinitions.size(); i++)
            fundefinitions.elementAt(i).Accept(this);
        return null;
    }   /* DONE */
    
    public Object VisitReturnStatement(ASTReturnStatement returnstatement) {
        //Compare return type of specific function in environment and type of the expression being returned.
        //Error if they don't match.
        //retrieve the type of the function by looking up return in the variable environment
        VariableEntry returnEntry = variableEnv.find("return");
        //compare this type with the typeof returnstatement
        Type returntype = (Type) returnstatement.value().Accept(this);
        if (returntype != returnEntry.type()) {
            CompError.message(returnstatement.line(), "Return statement type "
                              + "does not match with the type given to the function.");
        }
        return null;
    }   /* DONE */
    
    /**
     * Adds a description of this function to the function environment
     * - Type of each parameter
     * - Return type of the function
     * 
     * @param prototype
     */
    public Object VisitPrototype(ASTPrototype prototype) {
        //Add prototype to function environment
        Vector<Type> params = new Vector<Type>();
        if (prototype.formals() != null) {
            Type paramType;
            for (int i=0; i < prototype.formals().size(); i++) {
                paramType = (Type) prototype.formals().elementAt(i).Accept(this);
                params.add(paramType);
            }
        }
        
        return null;
    }   /* DONE */
    
    public Object VisitUnaryOperatorExpression(ASTUnaryOperatorExpression unaryexpression) {
        //Deal with logical "NOT"
        
        Type operand = (Type) unaryexpression.operand().Accept(this);
        
        switch (unaryexpression.operator()) {
            case ASTUnaryOperatorExpression.BAD_OPERATOR:    // Is this necessary?
                return IntegerType.instance();
                
            case ASTUnaryOperatorExpression.NOT:
                if (operand != BooleanType.instance()) {
                    CompError.message(unaryexpression.line(), "NOT operators requires "
                            + "a boolean operand");
                    return IntegerType.instance();
                }
                return BooleanType.instance();
        }
        return IntegerType.instance();
    }   /* DONE */
    
    public Object VisitStatements(ASTStatements statements) {
        for (int i = 0; i<statements.size(); i++) {
            statements.elementAt(i).Accept(this);
        }
        return null;
    }   /* DONE */
    
    public Object VisitVariableExpression(ASTVariableExpression varexpression) {
        Type variableexpression = (Type) varexpression.variable().Accept(this);
        return variableexpression;
    }   /* DONE */
    
    public Object VisitVariableDefStatement(ASTVariableDefStatement varstatement) {
        //Checks Type
        insertArrayType(varstatement.type(), varstatement.arraydimension(), varstatement.line());
        //Check variable name
        if (variableEnv.find(varstatement.name()) != null) {
            CompError.message(varstatement.line(), "Duplicate local variable " + 
                    varstatement.name() + ".");
        }
        return null;
    }   /* DONE */

    public Object VisitProgram(ASTProgram program) {
        program.classes().Accept(this);
        program.functiondefinitions().Accept(this);
        return null;
    }   /* DONE */
    
    public Object VisitWhileStatement(ASTWhileStatement whilestatement) {
        System.out.println("While (test/body)");
        Type test = (Type) whilestatement.test().Accept(this);

        if (test != BooleanType.instance()) {
            CompError.message(whilestatement.line(), "While test must be a boolean");
        }

        if (whilestatement.body() != null) {
            variableEnv.beginScope();
            whilestatement.body().Accept(this);
            variableEnv.endScope();
        }
        return null;
        /* DONE */

    }

    public Object VisitIntegerLiteral(ASTIntegerLiteral literal) {

        return IntegerType.instance();
    }   /* DONE */

    public Object VisitBaseVariable(ASTBaseVariable base) {
        VariableEntry baseEntry = variableEnv.find(base.name());
        if (baseEntry == null) {
            CompError.message(base.line(), "Variable " + base.name() + " is not defined in this " +
                    "scope");
            return IntegerType.instance();
        } else {
            return baseEntry.type();
        }
    }   /* DONE */

    public Object VisitIfStatement(ASTIfStatement ifsmt) {

        Type test = (Type) ifsmt.test().Accept(this);

        if (test != BooleanType.instance()) {
            CompError.message(ifsmt.line(), "If test must be a boolean");
        }

        ifsmt.thenstatement().Accept(this);

        if(ifsmt.elsestatement() != null) {
            ifsmt.elsestatement().Accept(this);
        }

        return null;
    }   //DONE (do not have to account for () right?
}
