import com.sun.jdi.IntegerType;
import jdk.nashorn.internal.codegen.types.BooleanType;

import java.util.Vector;

public class SemanticAnalyzer implements ASTVisitor {

    private VariableEnvironment variableEnv;
    private FunctionEnvironment functionEnv;
    private TypeEnvironment typeEnv;

    int indentlevel = 0;


    public SemanticAnalyzer() {
        variableEnv = new VariableEnvironment();
        functionEnv = new FunctionEnvironment();
        functionEnv.addBuiltinFunctions();
        typeEnv = new TypeEnvironment();
    }
    
    public Object VisitNewArrayExpression(ASTNewArrayExpression newarrayexpression) {
        newarrayexpression.elements().Accept(this);
        return null;
    }
    
    public Object VisitClasses(ASTClasses classes){
        for (i = 0; i <classes.size(); i++) {
            classes.elementAt(i).Accept(this);
        }
        return null;
    }
    
    public Object VisitAssignmentStatement(ASTAssignmentStatement assignstatement) {
        Type lhs = (Type) assignstatement.variable().Accept(this);
        Type rhs = (Type) assignstatement.value().Accept(this);
        
        if (lhs != rhs) {
            CompError.message(assignstatement.line(), "Lefthand side and righthand "
                    + "side of and assignement statement must match.");
        }
        return null;
    }
    
    public Object VisitArrayVariable(ASTArrayVariable arrayvariable) {
        array.base().Accept(this);
        array.index().Accept(this);
        return null;
    }
    
    public Object VisitBooleanLiteral(ASTBooleanLiteral boolliteral) {
        //done
        return BooleanType.instance();
    }
    
    public Object VisitClass(ASTClass asclass) {
        if (asclass.variabledefs() != null) {
            asclass.variabledefs().Accept(this);
        }
        return null;
    }
    
    public Object VisitInstanceVariableDef(ASTInstanceVariableDef variabledef)
    {
        //this may apply to all but we need to store the bracket count to be
        //retrieved later from the parse table
        //what do we store here
        return null;
    }
    
    public Object VisitClassVariable(ASTClassVariable classvar){
        Type base = (Type) classvar.base().Accept(this);
        //TODO: cont.
        return null;
    }
    
    public Object VisitForStatement(ASTForStatement forstatement) {
        forstatement.initialize.Accept(this);
        forstatement.test().Accept(this);
        forstatement.increment().Accept(this);
        forstatement.body().Accept(this);
        return null;
    }
    
    public Object VisitEmptyStatement(ASTEmptyStatement emptystate) {
        //WHAT DO WE RETURN HERE
        return null;
    }
    
    public Object VisitDoWhileStatement(ASTDoWhileStatement dowhile) {
        //NO INDENT IS NECESSARY HERE
        dowhile.test().Accept(this);
        dowhile.body().Accept(this);
        return null;
    }
    
    public Object VisitFormal(ASTFormal formal) {
        //COUNT BRACKETS I THINK
        return null;
    }
    
    public Object VisitFormals(ASTFormals formals) {
        if ((formals == null) || formals.size() == 0)
            return null;
        for (i=0; i<formals.size(); i++) {
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
        for (i=0; i<functioncall.size(); i++)
            functioncall.elementAt(i).Accept(this);
        return null;
    }
    
    public Object VisitFunctionCallStatement(ASTFunctionCallStatement statement) {
        for (i=0; i<functioncall.size(); i++)
            functioncall.elementAt(i).Accept(this);
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
        int i;
        for (i=0; i<variabledefs.size(); i++) {
            variabledefs.elementAt(i).Accept(this);
        }
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
    }
    
    public Object VisitFunctionDefinitions(ASTFunctionDefinitions fundefinitions) {
        int i;
        for (i=0; i < functiondefs.size(); i++)
            functiondefs.elementAt(i).Accept(this);
        return null;
    }
    
    public Object VisitReturnStatement(ASTReturnStatement returnstatement) {
        //TODO: Compare return type of specific function in environment and type of the expression being returned.
        //      Error if they don't match.
        return null;
    }
    
    public Object VisitPrototype(ASTPrototype prototype) {
        if (prototype.formals() != null) {
            for (i=0; i < prototype.formals().size(); i++)
                prototype.formals().elementAt(i).Accept(this);

        }
        return null;
    }
    
    public Object VisitUnaryOperatorExpression(ASTUnaryOperatorExpression unaryexpression) {
        operator.operand().Accept(this);
        //Include negative sign?
        return null;
    }
    
    public Object VisitStatements(ASTStatements statements) {
        //May need indents but I don't think so
        for (i = 0; i<statements.size(); i++) {
            statements.elementAt(i).Accept(this);
        }
        return null;
    }
    
    public Object VisitVariableExpression(ASTVariableExpression varexpression) {
        varexpression.variable().Accept(this);
        return null;
    }
    
    public Object VisitVariableDefStatement(ASTVariableDefStatement varstatement) {
        indentlevel++;
        Type visitvariabletype = (Type) varstatement.test().Accept(this);

        if (visitvariabletype == BooleanType.instance() ||
                visitvariabletype == IntegerType.instance()) {
            CompError.message(varstatement.line, "Variable cannot be an int or a boolean.");
        }

        //if (varstatement)
        indentlevel--;

        return null;

    }

    public Object VisitProgram(ASTProgram program) {
        program.classes().Accept(this);
        program.functiondefinitions().Accept(this);
        return null;
    }
    
    public Object VisitWhileStatement(ASTWhileStatement whilestatement) {
        Print("While (test/body)");
        indentlevel++;
        Type test = (Type) whilestatement.test().Accept(this);

        if (test != BooleanType.instance()) {
            CompError.message(whilestatement.line(), "While test must be a boolean");
        }

        if (whilestatement.body() != null) {
            whilestatement.body().Accept(this);
        }
        indentlevel--;
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
