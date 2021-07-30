# ![](./images/sempare-logo-45px.png) Sempare Full Text Expression Demo

Copyright (c) 2021 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

A simple demo on how to parse a string that could be used for a full text search.

The unit tests illustate the usage.

The project was written in a few hours. I havn't split the lexer out or and it is used for both parsers where a specialisation could be used.

the two types of parsers are:
1. basic with expressions such as the following
```
		"hello"      ->    text = 'hello'
		"abc" or "def"   -> text = 'abc' or text ='def'
		"abc" "def"   -> text = 'abc' or text ='def'
		"hello%"      ->    text like 'hello%'
		
```
		you are more likely wanting to expand the string to use wildcards. 
		
		
		
		
2. more complex expressions with variables:
```
	name = "hello" and age > 10
```
	
		the demo pretty much just parses the expression and does a pretty print
		

the tests show examples how how to leverage additional operators like contains, startswith, endswith, between, etc


TODO: 
- make a bit more generic for customisation
- improve exception handling
- report location of lexing/parsing errors