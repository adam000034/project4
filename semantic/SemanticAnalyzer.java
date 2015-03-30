import java.util.Vector;

public class SemanticAnalyzer implements ASTVisitor {

    private VariableEnvironment variableEnv;
    private FunctionEnvironment functionEnv;
    private TypeEnvironment typeEnv;

    public SemanticAnalyzer() {
        variableEnv = new VariableEnvironment();
        functionEnv = new FunctionEnvironment();
        functionEnv.addBuiltinFunctions();
        typeEnv = new TypeEnvironment();
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
        if (typeEnv.find(type) == null) {
            CompError.message(linenum, "Base Type does not exist.");
            return IntegerType.instance();
        }
        String thisType = type;
        for (int i = 0; i < arraydimension; i++) {
            thisType += "[]";
        }
        ArrayType entry = (ArrayType) typeEnv.find(thisType);
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
        
    public Object VisitClasses(ASTClasses classes){
        for (int i = 0; i <classes.size(); i++) {
            classes.elementAt(i).Accept(this);
        }
        return null;
    }   /* DONE */
    
    public Object VisitAssignmentStatement(ASTAssignmentStatement assignstatement) {
        Type lhs = (Type) assignstatement.variable().Accept(this);
        Type rhs = (Type) assignstatement.value().Accept(this);
        
        if (lhs != rhs) {
            CompError.message(assignstatement.line(), "Lefthand side and righthand "
                    + "side of and assignement statement must match.");
        }
        return null;
    }   /* DONE */
    
    public Object VisitArrayVariable(ASTArrayVariable arrayvariable) {
        arrayvariable.base().Accept(this);
        arrayvariable.index().Accept(this);
        //TODO: Check if index goes over size?
        return null;
    }
    
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
        return insertArrayType(variabledef.type(), variabledef.arraydimension(), variabledef.line());
        //this may apply to all but we need to store the bracket count to be
        //retrieved later from the parse table
        //what do we store here
    }   /* DONE */
    
    public Object VisitClassVariable(ASTClassVariable classvar){
        Type base = (Type) classvar.base().Accept(this);
        //TODO: cont.
        return null;
    }
    
    public Object VisitForStatement(ASTForStatement forstatement) {
        variableEnv.beginScope();
        forstatement.initialize().Accept(this);
        forstatement.test().Accept(this);
        forstatement.increment().Accept(this);
        forstatement.body().Accept(this);
        variableEnv.endScope();
        return null;
    }
    
    public Object VisitEmptyStatement(ASTEmptyStatement emptystate) {
        return null;
    }   /* DONE */
    
    public Object VisitDoWhileStatement(ASTDoWhileStatement dowhile) {
        dowhile.test().Accept(this);
        variableEnv.beginScope();
        dowhile.body().Accept(this);
        variableEnv.endScope();
        return null;
    }
    
    public Object VisitFormal(ASTFormal formal) {
        return insertArrayType(formal.type(), formal.arraydimension(), formal.line());
    }   /* Done */
    
    public Object VisitFormals(ASTFormals formals) {
        if ((formals == null) || formals.size() == 0)
            return null;
        for (int i=0; i<formals.size(); i++) {
            formals.elementAt(i).Accept(this);
        }
        return null;
    }
    
    public Object VisitFunction(ASTFunction function) {
        if (function.formals() != null)
            function.formals().Accept(this);
        function.body().Accept(this);
        return null;
    }
    
    public Object VisitFunctionCallExpression(ASTFunctionCallExpression callexpression) {
        for (int i=0; i<callexpression.size(); i++)
            callexpression.elementAt(i).Accept(this);
        return null;
    }
    
    public Object VisitFunctionCallStatement(ASTFunctionCallStatement statement) {
        for (int i=0; i<statement.size(); i++)
            statement.elementAt(i).Accept(this);
        return null;
    }
    
    public Object VisitInstanceVariableDefs(ASTInstanceVariableDefs variabledefs) {
        int i;
        for (i=0; i<variabledefs.size(); i++) {
            variabledefs.elementAt(i).Accept(this);
        }
        return null;
    }
    
    public Object VisitNewClassExpression(ASTNewClassExpression classexpression) {
        /*int i;
        for (i=0; i<variabledefs.size(); i++) {
            variabledefs.elementAt(i).Accept(this);
        }*/
        return null;
    }
    
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
        int i;
        for (i=0; i < fundefinitions.size(); i++)
            fundefinitions.elementAt(i).Accept(this);
        return null;
    }
    
    public Object VisitReturnStatement(ASTReturnStatement returnstatement) {
        //TODO: Compare return type of specific function in environment and type of the expression being returned.
        //      Error if they don't match.
        return null;
    }
    
    public Object VisitPrototype(ASTPrototype prototype) {
        if (prototype.formals() != null) {
            for (int i=0; i < prototype.formals().size(); i++)
                prototype.formals().elementAt(i).Accept(this);

        }
        return null;
    }
    
    public Object VisitUnaryOperatorExpression(ASTUnaryOperatorExpression unaryexpression) {
        unaryexpression.operand().Accept(this);
        //Deal with logical "NOT"
        //Include negative sign?
        return null;
    }
    
    public Object VisitStatements(ASTStatements statements) {
        //May need indents but I don't think so
        for (int i = 0; i<statements.size(); i++) {
            statements.elementAt(i).Accept(this);
        }
        return null;
    }
    
    public Object VisitVariableExpression(ASTVariableExpression varexpression) {
        varexpression.variable().Accept(this);
        return null;
    }
    
    public Object VisitVariableDefStatement(ASTVariableDefStatement varstatement) {
        Type visitvariabletype = (Type) varstatement.Accept(this);

        if (visitvariabletype == BooleanType.instance() ||
                visitvariabletype == IntegerType.instance()) {
            CompError.message(varstatement.line(), "Variable cannot be an int or a boolean.");
        }

        //if (varstatement)

        return null;

    }

    public Object VisitProgram(ASTProgram program) {
        program.classes().Accept(this);
        program.functiondefinitions().Accept(this);
        return null;
    }
    
    public Object VisitWhileStatement(ASTWhileStatement whilestatement) {
        System.out.println("While (test/body)");
        Type test = (Type) whilestatement.test().Accept(this);

        if (test != BooleanType.instance()) {
            CompError.message(whilestatement.line(), "While test must be a boolean");
        }

        if (whilestatement.body() != null) {
            whilestatement.body().Accept(this);
        }
        return null;

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
    }
}
