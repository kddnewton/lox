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

      def get(name, line_number)
        if fields.key?(name)
          fields[name]
        elsif (method = klass.find_method(name))
          method.bind(self)
        else
          raise Error::RuntimeError.new("Undefined property '#{name}'.", line_number)
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
