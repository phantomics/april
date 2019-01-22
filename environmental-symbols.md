<!-- TITLE/ -->

## April APL Environmental Symbols

<!-- /TITLE -->

These symbols represent values defining the environment in which April code runs.

### System Variables

These variables can be reassigned by the user; for instance, `⎕io←0` sets the index origin to 0.

|Symbol|Name             |Description|
|------|-----------------|-----------|
|`⎕IO` |Index Origin     |The number from which counting begins; may be 1 (default) or 0.|
|`⎕PP` |Print Precision  |Precision at which decimal values are expressed for printing; default 10.|

### System Constants

The values of these symbols are fixed and cannot be changed.

|Symbol|Name             |Description|
|------|-----------------|-----------|
|`⎕AV` |Atomic Vector    |String of Unicode characters recognized by the language; used by ⍒⍋ grade functions and ⍸ index by function.|
|`⎕A`  |Alphabet Vector  |String of capital letter characters of the English Roman alphabet.|
|`⎕D`  |Digit Vector     |String of numeric characters from 0 to 9.|

### System Functions

These symbols represent standard functions available within April. Currently, the only system function is `⎕TS`, which takes no arguments and returns a timestamp vector.

|Symbol|Name             |Description|
|------|-----------------|-----------|
|`⎕TS` |Timestamp        |Evaluates to the current APL timestamp; a vector of 7 values expressing year, month, day, hour, minute, second and millisecond.|
