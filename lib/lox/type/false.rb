# frozen_string_literal: true

module Lox
  module Type
    # This represents the false value.
    class False < Object
      def self.instance
        @instance ||= new
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def to_lox
        "false"
      end

      def truthy?
        false
      end

      #-------------------------------------------------------------------------
      # Callable methods
      #-------------------------------------------------------------------------

      def ==(other)
        Type.boolean((other in False))
      end
    end
  end
end
