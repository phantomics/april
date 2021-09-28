<!-- TITLE/ -->

## April APL Environmental Symbols

<!-- /TITLE -->

These symbols represent values defining the environment in which April code runs.

### System Variables

These variables can be reassigned by the user; for instance, `⎕io←0` sets the index origin to 0.

|Symbol|Name                |Description|
|-------|--------------------|-----------|
|`⎕IO`  |Index Origin        |The number from which counting begins; may be 1 (default) or 0.|
|`⎕CT`  |Comparison Tolerance|Difference between floating point numbers below which they are considered equal.|
|`⎕PP`  |Print Precision     |Precision at which decimal values are expressed for printing; default 10.|
|`⎕DIV` |Division Method     |Defines how division by 0 is handled; may be 0 (default) or 1.|

### April's Unique System Variables

These are system variables exclusive to April, not found in any other variant of APL.

|Symbol|Name             |Description|
|------|-----------------|-----------|
|`⎕OST`|Output Stream    |The Lisp output stream to which formatted console output is sent.|

### System Constants

The values of these symbols are fixed and cannot be changed.

|Symbol|Name           |Description|
|------|---------------|-----------|
|`⎕A`  |Alphabet Vector|String of capital letter characters of the English Roman alphabet.|
|`⎕D`  |Digit Vector   |String of numeric characters from 0 to 9.|

### System Functions

These symbols represent standard functions available within April. Currently, the only system function is `⎕TS`, which takes no arguments and returns a timestamp vector.

|Symbol|Name             |Description|
|------|----------------|-----------|
|`⎕TS` |Timestamp       |Evaluates to the current APL timestamp: a vector of 7 values expressing year, month, day, hour, minute, second and millisecond.|
|`⎕NS` |Create Namespace|Create an empty namespace.|
|`⎕FMT`|Format          |Create a character matrix containing the printed representation of an array.|

### April's Unique System Functions

|Symbol |Name             |Description|
|-------|-----------------|-----------|
|`⎕DT`  |Coerce/Get Type  |Get a number representing the element type of an array or coerce an array to a particular element type.|