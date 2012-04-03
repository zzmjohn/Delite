/* ===========================================================
 * bootstrap-popover-MOD.js v2.0.2
 * http://twitter.github.com/bootstrap/javascript.html#popovers
 * ===========================================================
 * Copyright 2012 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * =========================================================== */


!function( $ ) {

 "use strict"

  var PopoverMod = function ( element, options ) {
    this.init('popoverMod', element, options)
  }

  /* NOTE: POPOVER EXTENDS BOOTSTRAP-TOOLTIP.js
     ========================================== */

  PopoverMod.prototype = $.extend({}, $.fn.popover.Constructor.prototype, {

    constructor: PopoverMod

  , show: function () {
      var $tip
        , inside
        , pos
        , actualWidth
        , actualHeight
        , placement
        , tp

      if (this.hasContent() && this.enabled) {
        $tip = this.tip()
        this.setContent()

        if (this.options.animation) {
          $tip.addClass('fade')
        }


        placement = typeof this.options.placement == 'function' ?
          this.options.placement.call(this, $tip[0], this.$element[0]) :
          this.options.placement


        inside = /in/.test(placement)

        $tip
          .remove()
          .css({ top: 0, left: 0, display: 'block' })
          .appendTo(inside ? this.$element : document.body)

        pos = this.getPosition(inside)

        actualWidth = $tip[0].offsetWidth
        actualHeight = $tip[0].offsetHeight

        //if manualPosition, get coordinates from position attribute
        if(this.options.manualPosition){
          tp = {top: this.options.position.top, left: this.options.position.left}
        }else{
          switch (inside ? placement.split(' ')[1] : placement) {
            case 'bottom':
              tp = {top: pos.top + pos.height, left: pos.left + pos.width / 2 - actualWidth / 2}
              break
            case 'top':
                tp = {top: pos.top - actualHeight, left: pos.left + pos.width / 2 - actualWidth / 2}
                break
            case 'left':
                  tp = {top: pos.top + pos.height / 2 - actualHeight / 2, left: pos.left - actualWidth}
                  break
            case 'right':
                    tp = {top: pos.top + pos.height / 2 - actualHeight / 2, left: pos.left + pos.width}
                    break
          }
        }

        $tip
          .css(tp)
          .addClass(placement)
          .addClass('in')
      }
    }

  })


 /* POPOVER PLUGIN DEFINITION
  * ======================= */

  $.fn.popoverMod = function ( option ) {
    return this.each(function () {
      var $this = $(this)
        , data = $this.data('popoverMod')
        , options = typeof option == 'object' && option
      if (!data) $this.data('popoverMod', (data = new PopoverMod(this, options)))
      if (typeof option == 'string') data[option]()
    })
  }

  $.fn.popoverMod.Constructor = PopoverMod

  $.fn.popoverMod.defaults = $.extend({} , $.fn.popover.defaults, {
   manualPosition: false
  , position: {top:0, left:0}
  })

}( window.jQuery );
