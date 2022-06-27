# frozen_string_literal: true

module Lox
  # This module knows how to walk through the syntax tree.
  module Visitor
    # A class that knows how to walk down the syntax tree.
    class BaseVisitor
      def visit(node)
        node&.accept(self)
      end

      def visit_all(nodes)
        nodes.map { |node| visit(node) }
      end

      def visit_child_nodes(node)
        visit_all(node.child_nodes)
      end

      # Visit an Assignment node.
      alias visit_assignment visit_child_nodes

      # Visit a Binary node.
      alias visit_binary visit_child_nodes

      # Visit a BlockStatement node.
      alias visit_block_statement visit_child_nodes

      # Visit an Expression node.
      alias visit_expression visit_child_nodes

      # Visit a ForStatement node.
      alias visit_for_statement visit_child_nodes

      # Visit a Group node.
      alias visit_group visit_child_nodes

      # Visit an IfStatement node.
      alias visit_if_statement visit_child_nodes

      # Visit a Literal node.
      alias visit_literal visit_child_nodes

      # Visit a Missing node.
      alias visit_missing visit_child_nodes

      # Visit a Print node.
      alias visit_print_statement visit_child_nodes

      # Visit a Program node.
      alias visit_program visit_child_nodes

      # Visit a Unary node.
      alias visit_unary visit_child_nodes

      # Visit a Variable node.
      alias visit_variable visit_child_nodes

      # Visit a VariableDeclaration node.
      alias visit_variable_declaration visit_child_nodes

      # Visit a WhileStatement node.
      alias visit_while_statement visit_child_nodes
    end
  end
end
