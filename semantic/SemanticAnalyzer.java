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
    
    public Object VisitNewArrayExpression(ASTNewArrayExpression newarrayexpression) {
        return null;
    }
    
    public Object VisitClasses(ASTClasses classes){
        return null;
    }
    
    public Object VisitAssignmentStatement(ASTAssignmentStatement assignstatement) {
        return null;
    }
    
    public Object VisitArrayVariable(ASTArrayVariable arrayvariable) {
        return null;
    }
    
    public Object VisitBooleanLiteral(ASTBooleanLiteral boolliteral) {
        return null;
    }
    
    public Object VisitClass(ASTClass asclass) {
        return null;
    }
    
    public Object VisitInstanceVariableDef(ASTInstanceVariableDef variabledef) {
        return null;
    }
    
    public Object VisitClassVariable(ASTClassVariable classvar){
        return null;
    }
    
    public Object VisitForStatement(ASTForStatement forstatement) {
        return null;
    }
    
    public Object VisitEmptyStatement(ASTEmptyStatement emptystate) {
        return null;
    }
    
    public Object VisitDoWhileStatement(ASTDoWhileStatement dowhile) {
        return null;
    }
    
    public Object VisitFormal(ASTFormal formal) {
        return null;
    }
    
    public Object VisitFormals(ASTFormals formals) {
        return null;
    }
    
    public Object VisitFunction(ASTFunction function) {
        return null;
    }
    
    public Object VisitFunctionCallExpression(ASTFunctionCallExpression callexpression) {
        return null;
    }
    
    public Object VisitFunctionCallStatement(ASTFunctionCallStatement statement) {
        return null;
    }
    
    public Object VisitInstanceVariableDefs(ASTInstanceVariableDefs variabledefs) {
        return null;
    }
    
    public Object VisitNewClassExpression(ASTNewClassExpression classexpression) {
        return null;
    }
    
    public Object VisitOperatorExpression(ASTOperatorExpression opexpression) {
        return null;
    }
    
    public Object VisitFunctionDefinitions(ASTFunctionDefinitions fundefinitions) {
        return null;
    }
    
    public Object VisitReturnStatement(ASTReturnStatement returnstatement) {
        return null;
    }
    
    public Object VisitPrototype(ASTPrototype prototype) {
        return null;
    }
    
    public Object VisitUnaryOperatorExpression(ASTUnaryOperatorExpression unaryexpression) {
        return null;
    }
    
    public Object VisitStatements(ASTStatements statements) {
        return null;
    }
    public Object VisitVariableExpression(ASTVariableExpression varexpression) {
        return null;
    }
    
    public Object VisitVariableDefStatement(ASTVariableDefStatement varstatement) {
        return null;
    }

    public Object VisitProgram(ASTProgram program) {
        program.classes().Accept(this);
        program.functiondefinitions().Accept(this);
        return null;
    }
    
    public Object VisitWhileStatement(ASTWhileStatement whilestatement) {
        return null;
    }

    public Object VisitIntegerLiteral(ASTIntegerLiteral literal) {
        return IntegerType.instance();
    }

    public Object VisitBaseVariable(ASTBaseVariable base) {
        VariableEntry baseEntry = variableEnv.find(base.name());
        if (baseEntry == null) {
            CompError.message(base.line(), "Variable " + base.name() + " is not defined in this " +
                    "scope");
            return IntegerType.instance();
        } else {
            return baseEntry.type();
        }
    }

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
