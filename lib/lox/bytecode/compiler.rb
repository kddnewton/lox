# frozen_string_literal: true

module Lox
  # This module is responsible for defining each of the bytecode instructions.
  module Bytecode
    # A class responsible for handling the stream coming from the parser and
    # compiling it into bytecode instructions.
    class Compiler
      attr_reader :source, :chunk

      def initialize(source)
        @source = source
        @chunk = Chunk.new(name: "main")
      end

      def on_assignment(variable:, value:, location:)
        chunk.op_set_global(line_number: line_number(location.start))
      end

      def on_binary(left:, operator:, right:, location:)
        line_number = line_number(location.start)

        case operator
        in { type: :PLUS }
          chunk.op_add(line_number:)
        in { type: :MINUS }
          chunk.op_subtract(line_number:)
        in { type: :STAR }
          chunk.op_multiply(line_number:)
        in { type: :SLASH }
          chunk.op_divide(line_number:)
        in { type: :BANG_EQUAL }
          chunk.op_equal(line_number:)
          chunk.op_not(line_number:)
        in { type: :EQUAL_EQUAL }
          chunk.op_equal(line_number:)
        in { type: :GREATER }
          chunk.op_greater(line_number:)
        in { type: :GREATER_EQUAL }
          chunk.op_less(line_number:)
          chunk.op_not(line_number:)
        in { type: :LESS }
          chunk.op_less(line_number:)
        in { type: :LESS_EQUAL }
          chunk.op_greater(line_number:)
          chunk.op_not(line_number:)
        end
      end

      def on_expression_statement(value:, location:)
        chunk.op_pop(line_number: line_number(location.start))
      end

      def on_false(location:)
        chunk.op_false(line_number: line_number(location.start))
      end

      def on_group(node:, location:)
      end

      def on_nil(location:)
        chunk.op_nil(line_number: line_number(location.start))
      end

      def on_number(value:, location:)
        chunk.op_constant(constant: Type::Number.new(value: value), line_number: line_number(location.start))
      end

      def on_print_statement(value:, location:)
        chunk.op_print(line_number: line_number(location.start))
      end

      def on_program(statements:, location:)
        chunk.op_return(line_number: line_number(location.start))
      end

      def on_string(value:, location:)
        chunk.op_constant(constant: Type::String.new(value: value), line_number: line_number(location.start))
      end

      def on_true(location:)
        chunk.op_true(line_number: line_number(location.start))
      end

      def on_unary_expression(operator:, node:, location:)
        line_number = line_number(location.start)

        case operator
        in { type: :BANG }
          chunk.op_not(line_number:)
        in { type: :MINUS }
          chunk.op_negate(line_number:)
        end
      end

      def on_variable(name:, next_token:, location:)
        chunk.op_constant(constant: Type::String.new(value: name), line_number: line_number(location.start))
        chunk.op_get_global(line_number: line_number(location.start)) unless next_token in { type: :EQUAL }
      end

      def on_variable_declaration(name:, initializer:, location:)
        chunk.op_nil(line_number: line_number(location.start)) unless initializer
        chunk.op_constant(constant: Type::String.new(value: name), line_number: line_number(location.start))
        chunk.op_define_global(line_number: line_number(location.start))
      end

      private

      def line_number(offset)
        source[0...offset].count("\n") + 1
      end
    end
  end
end
