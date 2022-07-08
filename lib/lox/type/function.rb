# frozen_string_literal: true

module Lox
  module Type
    class Function < Object
      attr_reader :descriptor, :closure, :arity, :is_init, :callable

      def initialize(descriptor:, closure:, arity: 0, is_init: false, &callable)
        @descriptor = descriptor
        @arity = arity
        @closure = closure
        @is_init = is_init
        @callable = callable
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def bind(instance)
        Function.new(
          descriptor: descriptor,
          closure: Visitor::Interpreter::Environment.new(parent: closure, variables: { this: instance }),
          arity: arity,
          is_init: is_init,
          &callable
        )
      end

      def call(*arguments)
        result = callable.call(closure, *arguments)
        is_init ? closure.variables[:this] : result
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
