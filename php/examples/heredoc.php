<?php

foo(<<< HEREDOC
HEREDOC line 1.
HEREDOC line 2;
HEREDOC
);

foo(<<< 'NOWDOC'
Nowdoc line 1.
Nowdoc line 2.
NOWDOC
);

$str1 = <<<HEREDOC1
Hello world!
HEREDOC1;

$str2 = <<<TXT
HEREDOC TEXT
TXT
;

?>