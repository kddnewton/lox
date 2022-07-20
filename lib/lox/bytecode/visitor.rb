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
  end
end
