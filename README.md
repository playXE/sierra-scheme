# Sierra Scheme

Small scheme VM featuring interpreter and JIT compiler.

## Pipeline

Lexer -> Reader (convert tokens to S-expressions) -> SynPass (expand macros and verify S-expressions) -> Bytecompiler (compile S-epxressions to bytecode) -> Interpreter -> JIT compiler.