# frozen_string_literal: true

module Lox
  # This module is responsible for defining each of the bytecode instructions.
  module Bytecode
    # Generic visitor that knows how to visit each of the instructions in the
    # bytecode.
    class Visitor
      def visit(instruction)
        instruction.accept(self)
      end
    end

    # Responsible for visiting a set of instructions and converting them into
    # a disassembled output string.
    class Dissassembler < Visitor
      attr_reader :chunk

      def initialize(chunk:)
        @chunk = chunk
      end

      def disassemble
        puts "== %s ==" % chunk.name

        offset = 0
        chunk.instructions.each_with_index do |instruction, index|
          print "%04d " % offset

          if offset > 0 && chunk.line_numbers[index] == chunk.line_numbers[index - 1]
            print "   | "
          else
            print "%4d " % chunk.line_numbers[index]
          end

          offset += instruction.accept(self)
        end
      end

      # Visit an OpAdd instruction.
      def visit_add(instruction)
        puts "OP_ADD"
        1
      end

      # Visit an OpConstant instruction.
      def visit_constant(instruction)
        print "%-16s %4d '" % ["OP_CONSTANT", instruction.index]
        print_value(chunk.constants[instruction.index])
        puts "'"
        2
      end

      # Visit an OpDefineGlobal instruction.
      def visit_define_global(instruction)
        puts "OP_DEFINE_GLOBAL"
        1
      end

      # Visit an OpDivide instruction.
      def visit_divide(instruction)
        puts "OP_DIVIDE"
        1
      end

      # Visit an OpEqual instruction.
      def visit_equal(instruction)
        puts "OP_EQUAL"
        1
      end

      # Visit an OpFalse instruction.
      def visit_false(instruction)
        puts "OP_FALSE"
        1
      end

      # Visit an OpGetGlobal instruction.
      def visit_get_global(instruction)
        puts "OP_GET_GLOBAL"
        1
      end

      # Visit an OpGreater instruction.
      def visit_greater(instruction)
        puts "OP_GREATER"
        1
      end

      # Visit an OpLess instruction.
      def visit_less(instruction)
        puts "OP_LESS"
        1
      end

      # Visit an OpMultiply instruction.
      def visit_multiply(instruction)
        puts "OP_MULTIPLY"
        1
      end

      # Visit an OpNegate instruction.
      def visit_negate(instruction)
        puts "OP_NEGATE"
        1
      end

      # Visit an OpNil instruction.
      def visit_nil(instruction)
        puts "OP_NIL"
        1
      end

      # Visit an OpNot instruction.
      def visit_not(instruction)
        puts "OP_NOT"
        1
      end

      # Visit an OpPop instruction.
      def visit_pop(instruction)
        puts "OP_POP"
        1
      end

      # Visit an OpPrint instruction.
      def visit_print(instruction)
        puts "OP_PRINT"
        1
      end

      # Visit an OpReturn instruction.
      def visit_return(instruction)
        puts "OP_RETURN"
        1
      end

      # Visit an OpSubtract instruction.
      def visit_subtract(instruction)
        puts "OP_SUBTRACT"
        1
      end

      # Visit an OpTrue instruction.
      def visit_true(instruction)
        puts "OP_TRUE"
        1
      end

      private

      def print_value(object)
        case object
        in Type::False
          print "false"
        in Type::Nil
          print "nil"
        in Type::Number[value:]
          print "%g" % value
        in Type::String[value:]
          print "%s" % value
        in Type::True
          print "true"
        end
      end
    end

    # Responsible for visiting a set of instructions and evaluating them.
    class Evaluator < Visitor
      INTERPRET_OK = 0
      INTERPRET_COMPILE_ERROR = 1
      INTERPRET_RUNTIME_ERROR = 2

      STACK_MAX = 256

      attr_reader :chunk, :stack, :globals

      def initialize(chunk:)
        @chunk = chunk
        @stack = []
        @globals = {}
      end 

      def evaluate
        chunk.instructions.each { |instruction| visit(instruction) }
      end

      # Visit an OpAdd instruction.
      def visit_add(instruction)
        left, right = stack.pop(2)
        stack.push(left + right)
      end

      # Visit an OpConstant instruction.
      def visit_constant(instruction)
        stack.push(chunk.constants[instruction.index])
      end

      # Visit an OpDefineGlobal instruction.
      def visit_define_global(instruction)
        value, name = stack.pop(2)
        globals[name.value] = value
      end

      # Visit an OpDivide instruction.
      def visit_divide(instruction)
        left, right = stack.pop(2)
        stack.push(left / right)
      end

      # Visit an OpEqual instruction.
      def visit_equal(instruction)
        left, right = stack.pop(2)
        stack.push(left == right)
      end

      # Visit an OpFalse instruction.
      def visit_false(instruction)
        stack.push(Type::False.instance)
      end

      # Visit an OpGetGlobal instruction.
      def visit_get_global(instruction)
        stack.push(globals[stack.pop.value])
      end

      # Visit an OpGreater instruction.
      def visit_greater(instruction)
        left, right = stack.pop(2)
        stack.push(left > right)
      end

      # Visit an OpLess instruction.
      def visit_less(instruction)
        left, right = stack.pop(2)
        stack.push(left < right)
      end

      # Visit an OpMultiply instruction.
      def visit_multiply(instruction)
        left, right = stack.pop(2)
        stack.push(left * right)
      end

      # Visit an OpNegate instruction.
      def visit_negate(instruction)
        stack.push(-stack.pop)
      end

      # Visit an OpNil instruction.
      def visit_nil(instruction)
        stack.push(Type::Nil.instance)
      end

      # Visit an OpNot instruction.
      def visit_not(instruction)
        stack.push(!stack.pop)
      end

      # Visit an OpPop instruction.
      def visit_pop(instruction)
        stack.pop
      end

      # Visit an OpPrint instruction.
      def visit_print(instruction)
        puts stack.pop.to_lox
      end

      # Visit an OpReturn instruction.
      def visit_return(instruction)
        INTERPRET_OK
      end

      # Visit an OpSubtract instruction.
      def visit_subtract(instruction)
        left, right = stack.pop(2)
        stack.push(left - right)
      end

      # Visit an OpTrue instruction.
      def visit_true(instruction)
        stack.push(Type::True.instance)
      end
    end

    # A set of bytecode instructions.
    class Chunk
      attr_reader :name, :instructions, :line_numbers, :constants

      def initialize(name:, instructions: [], line_numbers: [], constants: [])
        @name = name
        @instructions = instructions
        @line_numbers = line_numbers
        @constants = constants
      end

      def disassemble
        Dissassembler.new(chunk: self).disassemble
      end

      def evaluate
        Evaluator.new(chunk: self).evaluate
      end

      def push_instruction(instruction:, line_number:)
        instructions << instruction
        line_numbers << line_number
      end
    end

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

    # A class responsible for handling the stream coming from the parser and
    # compiling it into bytecode instructions.
    class Compiler
      attr_reader :source, :chunk

      def initialize(source)
        @source = source
        @chunk = Chunk.new(name: "main")
      end

      def on_binary(left:, operator:, right:, location:)
        line_number = line_number(location)

        case operator
        in { type: :PLUS }
          chunk.push_instruction(instruction: Instructions::OpAdd.new, line_number:)
        in { type: :MINUS }
          chunk.push_instruction(instruction: Instructions::OpSubtract.new, line_number:)
        in { type: :STAR }
          chunk.push_instruction(instruction: Instructions::OpMultiply.new, line_number:)
        in { type: :SLASH }
          chunk.push_instruction(instruction: Instructions::OpDivide.new, line_number:)
        in { type: :BANG_EQUAL }
          chunk.push_instruction(instruction: Instructions::OpEqual.new, line_number:)
          chunk.push_instruction(instruction: Instructions::OpNot.new, line_number:)
        in { type: :EQUAL_EQUAL }
          chunk.push_instruction(instruction: Instructions::OpEqual.new, line_number:)
        in { type: :GREATER }
          chunk.push_instruction(instruction: Instructions::OpGreater.new, line_number:)
        in { type: :GREATER_EQUAL }
          chunk.push_instruction(instruction: Instructions::OpLess.new, line_number:)
          chunk.push_instruction(instruction: Instructions::OpNot.new, line_number:)
        in { type: :LESS }
          chunk.push_instruction(instruction: Instructions::OpLess.new, line_number:)
        in { type: :LESS_EQUAL }
          chunk.push_instruction(instruction: Instructions::OpGreater.new, line_number:)
          chunk.push_instruction(instruction: Instructions::OpNot.new, line_number:)
        end
      end

      def on_expression_statement(value:, location:)
        instruction = Instructions::OpPop.new
        chunk.push_instruction(instruction:, line_number: line_number(location))
      end

      def on_false(location:)
        instruction = Instructions::OpFalse.new
        chunk.push_instruction(instruction:, line_number: line_number(location))
      end

      def on_group(node:, location:)
      end

      def on_nil(location:)
        instruction = Instructions::OpNil.new
        chunk.push_instruction(instruction:, line_number: line_number(location))
      end

      def on_number(value:, location:)
        instruction = Instructions::OpConstant.new(index: chunk.constants.size)
        chunk.push_instruction(instruction:, line_number: line_number(location))
        chunk.constants << Type::Number.new(value: value)
      end

      def on_print_statement(value:, location:)
        instruction = Instructions::OpPrint.new
        chunk.push_instruction(instruction:, line_number: line_number(location))
      end

      def on_program(statements:, location:)
        instruction = Instructions::OpReturn.new
        chunk.push_instruction(instruction:, line_number: line_number(location))
      end

      def on_string(value:, location:)
        instruction = Instructions::OpConstant.new(index: chunk.constants.size)
        chunk.push_instruction(instruction:, line_number: line_number(location))
        chunk.constants << Type::String.new(value: value)
      end

      def on_true(location:)
        instruction = Instructions::OpTrue.new
        chunk.push_instruction(instruction:, line_number: line_number(location))
      end

      def on_unary_expression(operator:, node:, location:)
        instruction =
          case operator
          in { type: :BANG }
            Instructions::OpNot.new
          in { type: :MINUS }
            Instructions::OpNegate.new
          end

        chunk.push_instruction(instruction:, line_number: line_number(location))
      end

      def on_variable_declaration(name:, initializer:, location:)
        unless initializer
          instruction = Instructions::OpNil.new
          chunk.push_instruction(instruction:, line_number: line_number(location))
        end

        instruction = Instructions::OpConstant.new(index: chunk.constants.size)
        chunk.push_instruction(instruction:, line_number: line_number(location))
        chunk.constants << Type::String.new(value: name)

        instruction = Instructions::OpDefineGlobal.new
        chunk.push_instruction(instruction:, line_number: line_number(location))
      end

      private

      def line_number(location)
        source[0...location.start].count("\n") + 1
      end
    end
  end
end
