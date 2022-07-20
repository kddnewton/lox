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
        instruction = Instructions::OpSetGlobal.new
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
      end

      def on_binary(left:, operator:, right:, location:)
        line_number = line_number(location.start)

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
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
      end

      def on_false(location:)
        instruction = Instructions::OpFalse.new
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
      end

      def on_group(node:, location:)
      end

      def on_nil(location:)
        instruction = Instructions::OpNil.new
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
      end

      def on_number(value:, location:)
        instruction = Instructions::OpConstant.new(index: chunk.constants.size)
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
        chunk.constants << Type::Number.new(value: value)
      end

      def on_print_statement(value:, location:)
        instruction = Instructions::OpPrint.new
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
      end

      def on_program(statements:, location:)
        instruction = Instructions::OpReturn.new
        chunk.push_instruction(instruction:, line_number: line_number(location.finish))
      end

      def on_string(value:, location:)
        instruction = Instructions::OpConstant.new(index: chunk.constants.size)
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
        chunk.constants << Type::String.new(value: value)
      end

      def on_true(location:)
        instruction = Instructions::OpTrue.new
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
      end

      def on_unary_expression(operator:, node:, location:)
        instruction =
          case operator
          in { type: :BANG }
            Instructions::OpNot.new
          in { type: :MINUS }
            Instructions::OpNegate.new
          end

        chunk.push_instruction(instruction:, line_number: line_number(location.start))
      end

      def on_variable(name:, location:)
        instruction = Instructions::OpConstant.new(index: chunk.constants.size)
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
        chunk.constants << Type::String.new(value: name)

        unless Lexer.new(source[location.start..]).tokens.take(2).last in { type: :EQUAL }
          instruction = Instructions::OpGetGlobal.new
          chunk.push_instruction(instruction:, line_number: line_number(location.start))
        end
      end

      def on_variable_declaration(name:, initializer:, location:)
        unless initializer
          instruction = Instructions::OpNil.new
          chunk.push_instruction(instruction:, line_number: line_number(location.start))
        end

        instruction = Instructions::OpConstant.new(index: chunk.constants.size)
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
        chunk.constants << Type::String.new(value: name)

        instruction = Instructions::OpDefineGlobal.new
        chunk.push_instruction(instruction:, line_number: line_number(location.start))
      end

      private

      def line_number(offset)
        source[0...offset].count("\n") + 1
      end
    end
  end
end
