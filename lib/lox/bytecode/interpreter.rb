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
      def visit_add(instruction) = binary(instruction, :+)

      # Visit an OpConstant instruction.
      def visit_constant(instruction) = stack.push(chunk.constants[instruction.index])

      # Visit an OpDefineGlobal instruction.
      def visit_define_global(instruction)
        value, name = stack.pop(2)
        globals[name.value] = value
      end

      # Visit an OpDivide instruction.
      def visit_divide(instruction) = binary(instruction, :/)

      # Visit an OpEqual instruction.
      def visit_equal(instruction) = binary(instruction, :==)

      # Visit an OpFalse instruction.
      def visit_false(instruction) = stack.push(Type::False.instance)

      # Visit an OpGetGlobal instruction.
      def visit_get_global(instruction)
        name = stack.pop.value

        if globals.key?(name)
          stack.push(globals[name])
        else
          raise Error::RuntimeError.new("Undefined variable '#{name}'.", line_number(instruction))
        end
      end

      # Visit an OpGreater instruction.
      def visit_greater(instruction) = binary(instruction, :>)

      # Visit an OpLess instruction.
      def visit_less(instruction) = binary(instruction, :<)

      # Visit an OpMultiply instruction.
      def visit_multiply(instruction) = binary(instruction, :*)

      # Visit an OpNegate instruction.
      def visit_negate(instruction) = unary(instruction, :-@)

      # Visit an OpNil instruction.
      def visit_nil(instruction) = stack.push(Type::Nil.instance)

      # Visit an OpNot instruction.
      def visit_not(instruction) = unary(instruction, :"!")

      # Visit an OpPop instruction.
      def visit_pop(instruction) = stack.pop

      # Visit an OpPrint instruction.
      def visit_print(instruction) = puts stack.pop.to_lox

      # Visit an OpReturn instruction.
      def visit_return(instruction) = INTERPRET_OK

      # Visit an OpSetGlobal instruction.
      def visit_set_global(instruction)
        name, value = stack.pop(2)

        if globals.key?(name.value)
          globals[name.value] = value
          stack.push(value)
        else
          raise Error::RuntimeError.new("Undefined variable '#{name.value}'.", line_number(instruction))
        end
      end

      # Visit an OpSubtract instruction.
      def visit_subtract(instruction) = binary(instruction, :-)

      # Visit an OpTrue instruction.
      def visit_true(instruction) = stack.push(Type::True.instance)

      private

      def line_number(instruction)
        chunk.line_numbers[chunk.instructions.index(instruction)]
      end

      def unary(instruction, operation)
        stack.push(stack.pop.public_send(operation))
      rescue Error::RuntimeError => error
        raise Error::RuntimeError.new(error.message, line_number(instruction))
      end

      def binary(instruction, operation)
        left, right = stack.pop(2)
        stack.push(left.public_send(operation, right))
      rescue Error::RuntimeError => error
        raise Error::RuntimeError.new(error.message, line_number(instruction))
      end
    end
  end
end
