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
        instructions.each_with_index do |instruction, index|
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

      # Visit an OpDivide instruction.
      def visit_divide(instruction)
        puts "OP_DIVIDE"
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

      private

      def print_value(object)
        case object
        in Type::Number[value:]
          print "%g" % value
        end
      end
    end

    # Responsible for visiting a set of instructions and evaluating them.
    class Evaluator < Visitor
      INTERPRET_OK = 0
      INTERPRET_COMPILE_ERROR = 1
      INTERPRET_RUNTIME_ERROR = 2

      STACK_MAX = 256

      attr_reader :chunk, :stack, :offset

      def initialize(chunk:)
        @chunk = chunk
        @stack = []
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

      # Visit an OpDivide instruction.
      def visit_divide(instruction)
        left, right = stack.pop(2)
        stack.push(left / right)
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

      # Visit an OpReturn instruction.
      def visit_return(instruction)
        INTERPRET_OK
      end

      # Visit an OpSubtract instruction.
      def visit_subtract(instruction)
        left, right = stack.pop(2)
        stack.push(left - right)
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

      # A binary division instruction.
      class OpDivide
        def accept(visitor)
          visitor.visit_divide(self)
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
    end

    # A class responsible for handling the stream coming from the parser and
    # compiling it into bytecode instructions.
    class Compiler
      attr_reader :chunk

      def initialize
        @chunk = Chunk.new(name: "main")
      end

      def on_binary(left:, operator:, right:, location:)
        instruction =
          case operator
          in { type: :PLUS }
            Instructions::OpAdd.new
          in { type: :MINUS }
            Instructions::OpSubtract.new
          in { type: :STAR }
            Instructions::OpMultiply.new
          in { type: :SLASH }
            Instructions::OpDivide.new
          end

        chunk.push_instruction(instruction:, line_number: 0)
        instruction
      end

      def on_group(node:, location:)
      end

      def on_number(value:, location:)
        instruction = Instructions::OpConstant.new(index: chunk.constants.size)

        chunk.push_instruction(instruction:, line_number: 0)
        chunk.constants << Type::Number.new(value: value)
        instruction
      end

      def on_program(statements:, location:)
        instruction = Instructions::OpReturn.new

        chunk.push_instruction(instruction:, line_number: 0)
        instruction
      end

      def on_unary_expression(operator:, node:, location:)
        instruction =
          case operator
          in { type: :MINUS }
            Instructions::OpNegate.new
          end

        chunk.push_instruction(instruction:, line_number: 0)
        instruction
      end
    end
  end
end
