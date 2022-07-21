# frozen_string_literal: true

module Lox
  # This module is responsible for defining each of the bytecode instructions.
  module Bytecode
    # A set of bytecode instructions.
    class Chunk
      attr_reader :name, :instructions, :line_numbers, :constants

      def initialize(name:, instructions: [], line_numbers: [], constants: [])
        @name = name
        @instructions = instructions
        @line_numbers = line_numbers
        @constants = constants
      end

      #-------------------------------------------------------------------------
      # Visitor shortcuts
      #-------------------------------------------------------------------------

      def disassemble
        Disassembler.new(chunk: self).disassemble
      end

      def interpret
        Interpreter.new(chunk: self).interpret
      end

      #-------------------------------------------------------------------------
      # Instruction push shortcuts
      #-------------------------------------------------------------------------

      def op_add(line_number:) =           push(Instructions::OpAdd.new, line_number)
      def op_define_global(line_number:) = push(Instructions::OpDefineGlobal.new, line_number)
      def op_divide(line_number:) =        push(Instructions::OpDivide.new, line_number)
      def op_equal(line_number:) =         push(Instructions::OpEqual.new, line_number)
      def op_false(line_number:) =         push(Instructions::OpFalse.new, line_number)
      def op_get_global(line_number:) =    push(Instructions::GetGlobal.new, line_number)
      def op_greater(line_number:) =       push(Instructions::OpGreater.new, line_number)
      def op_less(line_number:) =          push(Instructions::OpLess.new, line_number)
      def op_multiply(line_number:) =      push(Instructions::OpMultiply.new, line_number)
      def op_negate(line_number:) =        push(Instructions::OpNegate.new, line_number)
      def op_nil(line_number:) =           push(Instructions::OpNil.new, line_number)
      def op_not(line_number:) =           push(Instructions::OpNot.new, line_number)
      def op_pop(line_number:) =           push(Instructions::OpPop.new, line_number)
      def on_print(line_number:) =         push(Instructions::OpPrint.new, line_number)
      def op_return(line_number:) =        push(Instructions::OpReturn.new, line_number)
      def op_set_global(line_number:) =    push(Instructions::OpSetGlobal.new, line_number)
      def op_subtract(line_number:) =      push(Instructions::OpSubtract.new, line_number)
      def op_true(line_number:) =          push(Instructions::OpTrue.new, line_number)

      def op_constant(constant:, line_number:)
        push(Instructions::OpConstant.new(index: constants.size), line_number)
        constants << constant
      end

      private

      def push(instruction, line_number)
        instructions << instruction
        line_numbers << line_number
      end
    end
  end
end
