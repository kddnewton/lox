# frozen_string_literal: true

module Lox
  module Type
    # This represents the true value.
    class True < Object
      def self.instance
        @instance ||= new
      end

      #-------------------------------------------------------------------------
      # Debug methods
      #-------------------------------------------------------------------------

      def to_lox
        "true"
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def !@
        False.instance
      end

      def ==(other)
        Type.boolean((other in True))
      end
    end
  end
end
