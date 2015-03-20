import jdk.nashorn.internal.codegen.types.BooleanType;

public class SemanticAnalyzer implements ASTVisitor {

    private VariableEnvironment vairableEnv;
    private FunctionEnvironment functionEnv;
    private TypeEnvironment typeEnv;


    public SemanticAnalyzer() {
        vairableEnv = new VariableEnvironment();
        functionEnv = new FunctionEnvironment();
        functionEnv.addBuiltinFunctions();
        typeEnv = new TypeEnvironment();
    }

    public Object VisitProgram(ASTProgram program) {
        program.classes().Accept(this);
        program.functiondefinitions().Accept(this);
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
