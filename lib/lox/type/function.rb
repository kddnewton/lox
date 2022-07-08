# frozen_string_literal: true

module Lox
  module Type
    class Function < Object
      attr_reader :descriptor, :arity, :closure, :callable

      def initialize(descriptor:, arity:, closure:, &callable)
        @descriptor = descriptor
        @arity = arity
        @closure = closure
        @callable = callable
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def bind(instance)
        Function.new(
          descriptor: descriptor,
          arity: arity,
          closure: Visitor::Interpreter::Environment.new(parent: closure, variables: { this: instance }),
          &callable
        )
      end

      def call(*arguments)
        callable.call(closure, *arguments)
      end

      def callable?
        true
      end

      def to_lox
        descriptor
      end

      #-------------------------------------------------------------------------
      # Callable methods
      #-------------------------------------------------------------------------

      def ==(other)
        Type.boolean(super)
      end
    end
  end
end
