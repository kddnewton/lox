# frozen_string_literal: true

module Lox
  # This module is responsible for defining each of the bytecode instructions.
  module Bytecode
    # A container module for all of the instruction definitions.
    module Instructions
      # A binary addition instruction.
      class OpAdd
        def accept(visitor)
          visitor.visit_add(self)
        end
      end

      # Load a constant value from the constant pool onto the stack.
      class OpConstant
        attr_reader :index

        def initialize(index:)
          @index = index
        end

        def accept(visitor)
          visitor.visit_constant(self)
        end
      end

      # Define a global variable.
      class OpDefineGlobal
        def accept(visitor)
          visitor.visit_define_global(self)
        end
      end

      # A binary division instruction.
      class OpDivide
        def accept(visitor)
          visitor.visit_divide(self)
        end
      end

      # Call == on the top two values on the stack.
      class OpEqual
        def accept(visitor)
          visitor.visit_equal(self)
        end
      end

      # Push false onto the stack.
      class OpFalse
        def accept(visitor)
          visitor.visit_false(self)
        end
      end

      # Get a global variable and push it onto the stack.
      class OpGetGlobal
        def accept(visitor)
          visitor.visit_get_global(self)
        end
      end

      # Call > on the top two values on the stack.
      class OpGreater
        def accept(visitor)
          visitor.visit_greater(self)
        end
      end

      # Call < on the top two values on the stack.
      class OpLess
        def accept(visitor)
          visitor.visit_less(self)
        end
      end

      # A binary multiplication instruction.
      class OpMultiply
        def accept(visitor)
          visitor.visit_multiply(self)
        end
      end

      # Negate a value.
      class OpNegate
        def accept(visitor)
          visitor.visit_negate(self)
        end
      end

      # Push nil onto the stack.
      class OpNil
        def accept(visitor)
          visitor.visit_nil(self)
        end
      end

      # Call ! on a value.
      class OpNot
        def accept(visitor)
          visitor.visit_not(self)
        end
      end

      # Pop a value off the stack.
      class OpPop
        def accept(visitor)
          visitor.visit_pop(self)
        end
      end

      # Print the top value on the stack.
      class OpPrint
        def accept(visitor)
          visitor.visit_print(self)
        end
      end

      # Return from the current function.
      class OpReturn
        def accept(visitor)
          visitor.visit_return(self)
        end
      end

      # Set a global variable.
      class OpSetGlobal
        def accept(visitor)
          visitor.visit_set_global(self)
        end
      end

      # A binary subtraction instruction.
      class OpSubtract
        def accept(visitor)
          visitor.visit_subtract(self)
        end
      end

      # Push true onto the stack.
      class OpTrue
        def accept(visitor)
          visitor.visit_true(self)
        end
      end
    end
  end
end
