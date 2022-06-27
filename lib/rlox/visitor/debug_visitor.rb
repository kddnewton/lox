# frozen_string_literal: true

module Lox
  module Visitor
    # This is a visitor that will print the tree to a set of s-expressions that
    # match what the book's test suite expects.
    class DebugVisitor < BaseVisitor
      # In debug methods, it's necessary to go from the operator to the value
      # that it represents in source.
      OPERATORS_NAMES = Lexer::OPERATORS.invert

      # Visit a Binary node.
      def visit_binary(node)
        "(#{OPERATORS_NAMES[node.operator.type]} #{visit(node.left)} #{visit(node.right)})"
      end

      # Visit a Group node.
      def visit_group(node)
        "(group #{visit(node.node)})"
      end

      # Visit a Literal node.
      def visit_literal(node)
        if node.value in Type::Number
          lox = node.value.to_lox
          lox.include?(".") ? lox : "#{lox}.0"
        else
          node.value.to_lox
        end
      end

      # Visit a Unary node.
      def visit_unary(node)
        "(#{OPERATORS_NAMES[node.operator.type]} #{visit(node.node)})"
      end
    end
  end
end
