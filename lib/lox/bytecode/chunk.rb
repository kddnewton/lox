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

      def disassemble
        Disassembler.new(chunk: self).disassemble
      end

      def interpret
        Interpreter.new(chunk: self).interpret
      end

      def push_instruction(instruction:, line_number:)
        instructions << instruction
        line_numbers << line_number
      end
    end
  end
end
