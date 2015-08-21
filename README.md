# Symbolic differentiation for Accelerate

This allows you to symbolically differentiate functions you've written with the
Accelerate DSL. The main use case for this is GPU accelerated deep
learning.

## Project status

  - Differentiate scalar functions
  - Differentiate some tuple functions of scalars
  - Not all of DSL supported
  - All primitive floating point operations supported
  - Tested against numerical differentiation

## Not done

  - Arrays
  - Arbitrary tuples

## Need help with

  - Differentiating arbitrary tuples or failing appropriately
  - Replacing dirty hacks like unsafeCoerce

