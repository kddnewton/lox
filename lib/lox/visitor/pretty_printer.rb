# frozen_string_literal: true

module Lox
  module Visitor
    # This is a visitor that will print the tree to a set of s-expressions that
    # match what the book's test suite expects.
    class PrettyPrinter < BaseVisitor
      # In debug methods, it's necessary to go from the operator to the value
      # that it represents in source.
      OPERATORS_NAMES = Lexer::OPERATORS.invert

      attr_reader :q

      def initialize(q)
        @q = q
      end

      # Visit an Assignment node.
      def visit_assignment(node)
        group("assignment") do
          q.breakable
          visit(node.variable)
          q.breakable
          visit(node.value)
        end
      end

      # Visit a Binary node.
      def visit_binary(node)
        group(OPERATORS_NAMES[node.operator.type]) do
          q.breakable
          visit(node.left)
          q.breakable
          visit(node.right)
        end
      end

      # Visit a BlockStatement node.
      def visit_block_statement(node)
        group("block-statement") do
          q.breakable
          q.seplist(node.statements) { |statement| visit(statement) }
        end
      end

      # Visit a Call node.
      def visit_call(node)
        group("call") do
          q.breakable
          visit(node.callee)

          if node.arguments.any?
            q.breakable
            q.seplist(node.arguments) { |argument| visit(argument) }
          end
        end
      end

      # Visit a ClassStatement node.
      def visit_class_statement(node)
        group("class") do
          q.breakable
          q.text(node.name.value)

          q.breakable
          q.seplist(node.methods) { |method| visit(method) }
        end
      end

      # Visit an Expression node.
      def visit_expression(node)
        group("expression") do
          q.breakable
          visit(node.value)
        end
      end

      # Visit a ForStatement node.
      def visit_for_statement(node)
        group("for") do
          q.breakable
          visit(node.initializer)

          q.breakable
          visit(node.condition)

          q.breakable
          visit(node.increment)

          q.breakable
          visit(node.body)
        end
      end

      # Visit a Function node.
      def visit_function(node)
        group("function") do
          q.breakable
          q.text(node.name)

          q.breakable
          q.seplist(node.parameters) { |parameter| q.text(parameter.value) }

          q.breakable
          q.seplist(node.statements) { |statement| visit(statement) }
        end
      end

      # Visit a GetExpression node.
      def visit_get_expression(node)
        group("get-expression") do
          q.breakable
          visit(node.object)

          q.breakable
          q.text(node.name)
        end
      end

      # Visit a Group node.
      def visit_group(node)
        group("group") do
          q.breakable
          visit(node.node)
        end
      end

      # Visit an IfStatement node.
      def visit_if_statement(node)
        group("if-statement") do
          q.breakable
          visit(node.condition)
          q.breakable
          visit(node.then_branch)

          if node.else_branch
            q.breakable
            visit(node.else_branch)
          end
        end
      end

      # Visit a Literal node.
      def visit_literal(node)
        if node.value in Type::Number
          lox = node.value.to_lox
          q.text(lox.include?(".") ? lox : "#{lox}.0")
        else
          q.text(node.value.to_lox)
        end
      end

      # Visit a Missing node.
      def visit_missing(node)
        q.text("(missing)")
      end

      # Visit a Print node.
      def visit_print_statement(node)
        group("print") do
          q.breakable
          visit(node.value)
        end
      end

      # Visit a Program node.
      def visit_program(node)
        group("program") do
          q.breakable
          q.seplist(node.statements) { |statement| visit(statement) }
        end
      end

      # Visit a ReturnStatement node.
      def visit_return_statement(node)
        group("return") do
          if node.value
            q.breakable
            visit(node.value)
          end
        end
      end

      # Visit a SetExpression node.
      def visit_set_expression(node)
        group("set-expression") do
          q.breakable
          visit(node.object)

          q.breakable
          q.text(node.name.value)

          q.breakable
          visit(node.value)
        end
      end

      # Visit a Unary node.
      def visit_unary(node)
        group(OPERATORS_NAMES[node.operator.type]) do
          q.breakable
          visit(node.node)
        end
      end

      # Visit a Variable node.
      def visit_variable(node)
        group("variable") do
          q.breakable
          q.text(node.name)
        end
      end

      # Visit a VariableDeclaration node.
      def visit_variable_declaration(node)
        group("variable-declaration") do
          q.breakable
          q.text(node.name)

          if node.initializer
            q.breakable
            visit(node.initializer)
          end
        end
      end

      # Visit a WhileStatement node.
      def visit_while_statement(node)
        group("while-statement") do
          q.breakable
          visit(node.condition)
          q.breakable
          visit(node.body)
        end
      end

      private

      def group(name)
        q.group do
          q.text("(#{name}")
          q.nest(2) do
            yield
          end
          q.breakable("")
          q.text(")")
        end
      end
    end
  end
end
