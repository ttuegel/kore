module CEIL-MAP

  imports DOMAINS

  configuration
    <T>
      <k> $PGM:Int </k>
      <env> .Map </env>
    </T>

  rule <k> K:Int => V:Int </k> <env> K |-> V ... </env>

endmodule