# frozen_string_literal: true

module Lox
  # This module is responsible for defining each of the bytecode instructions.
  module Bytecode
    # Responsible for visiting a set of instructions and evaluating them.
    class Interpreter < Visitor
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

      def interpret
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

      # Visit an OpSetGlobal instruction.
      def visit_set_global(instruction)
        name, value = stack.pop(2)
        globals[name.value] = value
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
  end
end
