# frozen_string_literal: true

module Lox
  # This module is responsible for defining each of the bytecode instructions.
  module Bytecode
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

      # Visit an OpSetGlobal instruction.
      def visit_set_global(instruction)
        puts "OP_SET_GLOBAL"
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
  end
end
