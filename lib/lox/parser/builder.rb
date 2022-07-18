# frozen_string_literal: true

module Lox
  class Parser
    # This class is responsible for responding to all of the methods necessary to
    # implement the parser interface and build out the AST.
    class Builder
      def on_assignment(variable:, value:, location:)
        AST::Assignment.new(variable:, value:, location:)
      end

      def on_binary(left:, operator:, right:, location:)
        AST::Binary.new(left:, operator:, right:, location:)
      end

      def on_block_statement(statements:, location:)
        AST::BlockStatement.new(statements:, location:)
      end

      def on_call(callee:, arguments:, arguments_location:, location:)
        AST::Call.new(callee:, arguments:, arguments_location:, location:)
      end

      def on_class_statement(name:, superclass:, methods:, location:)
        AST::ClassStatement.new(name:, superclass:, methods:, location:)
      end

      def on_expression_statement(value:, location:)
        AST::Expression.new(value:, location:)
      end

      def on_false(location:)
        AST::Literal.new(value: Type::False.instance, location: location)
      end

      def on_for_statement(initializer:, condition:, increment:, body:, location:)
        AST::ForStatement.new(initializer:, condition:, increment:, body:, location:)
      end

      def on_function(name:, parameters:, statements:, location:)
        AST::Function.new(name:, parameters:, statements:, location:)
      end

      def on_get_expression(object:, name:, location:)
        AST::GetExpression.new(object:, name:, location:)
      end

      def on_group(node:, location:)
        AST::Group.new(node:, location:)
      end

      def on_if_statement(condition:, then_branch:, else_branch:, location:)
        AST::IfStatement.new(condition:, then_branch:, else_branch:, location:)
      end

      def on_missing(location:)
        AST::Missing.new(location:)
      end

      def on_nil(location:)
        AST::Literal.new(value: Type::Nil.instance, location: location)
      end

      def on_number(value:, location:)
        AST::Literal.new(value: Type::Number.new(value: value), location: location)
      end

      def on_print_statement(value:, location:)
        AST::PrintStatement.new(value:, location:)
      end

      def on_program(statements:, location:)
        AST::Program.new(statements:, location:)
      end

      def on_return_statement(value:, location:)
        AST::ReturnStatement.new(value:, location:)
      end

      def on_set_expression(object:, name:, value:, location:)
        AST::SetExpression.new(object:, name:, value:, location:)
      end

      def on_string(value:, location:)
        AST::Literal.new(value: Type::String.new(value: value), location: location)
      end

      def on_super_expression(method:, location:)
        AST::SuperExpression.new(method:, location:)
      end

      def on_this_expression(location:)
        AST::ThisExpression.new(location: location)
      end

      def on_true(location:)
        AST::Literal.new(value: Type::True.instance, location: location)
      end

      def on_unary_expression(operator:, node:, location:)
        AST::Unary.new(operator:, node:, location:)
      end

      def on_variable(name:, location:)
        AST::Variable.new(name:, location:)
      end

      def on_variable_declaration(name:, initializer:, location:)
        AST::VariableDeclaration.new(name:, initializer:, location:)
      end

      def on_while_statement(condition:, body:, location:)
        AST::WhileStatement.new(condition:, body:, location:)
      end
    end
  end
end
