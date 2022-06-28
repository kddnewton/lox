# frozen_string_literal: true

module Lox
  module Type
    # This represents an instance of a class.
    class Instance < Object
      attr_reader :klass, :fields

      def initialize(klass:, fields: {})
        @klass = klass
        @fields = fields
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def get(name, location)
        if fields.key?(name)
          fields[name]
        else
          raise Error::RuntimeError.new("Undefined property '#{name}'.", location)
        end
      end

      def set(name, value)
        fields[name] = value
      end

      def to_lox
        "#{klass.name} instance"
      end

      #-------------------------------------------------------------------------
      # Callable methods
      #-------------------------------------------------------------------------

      def ==(other)
        Type::False.instance
      end
    end
  end
end