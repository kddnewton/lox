# frozen_string_literal: true

module Lox
  module Type
    # This represents the false value.
    class False < Object
      def self.instance
        @instance ||= new
      end

      #-------------------------------------------------------------------------
      # Debug methods
      #-------------------------------------------------------------------------

      def to_lox
        "false"
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def !@
        True.instance
      end

      def ==(other)
        Type.boolean((other in False))
      end
    end
  end
end
