# frozen_string_literal: true

module Lox
  module Type
    # This represents the nil value.
    class Nil < Object
      def self.instance
        @instance ||= new
      end

      #-------------------------------------------------------------------------
      # Debug methods
      #-------------------------------------------------------------------------

      def to_lox
        "nil"
      end

      def truthy?
        false
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def !@
        True.instance
      end

      def ==(other)
        Type.boolean((other in Nil))
      end
    end
  end
end
