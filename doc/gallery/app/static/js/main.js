window.addEventListener('pageshow', (event) => {
    if (event.persisted) {
        $('#loading-screen').hide();
    }
});
$(function() {
    window.cookieconsent.initialise({
        "palette": {
          "popup": {
            "background": "#000"
          },
          "button": {
            "background": "#F39619"
          }
        }
    });
    $('.app-container').on('click', function() {
        $('#loading-screen').show();
    });
    $('.btn-upvote').on('click', function() {
        this.disabled = true;
        var appId = this.dataset.id;
        $.post( "api/vote-up", { id: appId })
          .done(function( data ) {
            var noUpvotesEl = $('#' + appId + '_noupvotes');
            var noUpvotes = noUpvotesEl.text();
            noUpvotes = parseInt(noUpvotes.substring(1, noUpvotes.length - 1), 10) + 1;
            $('#' + appId + '_noupvotes').text('(' + noUpvotes + ')');
          });
    });
    $("#newAppForm").on('submit', function(e){
        e.preventDefault();
        $.ajax({
            type: 'POST',
            url: 'api/add-app',
            data: new FormData(this),
            dataType: 'json',
            contentType: false,
            cache: false,
            processData:false,
            beforeSend: function(){
                $('.submitBtn').attr("disabled","disabled");
                $('#newAppForm').css("opacity",".5");
            },
            success: function(response){
                $('.statusMsg').html('');
                if(response.status == 0){
                    $('#newAppForm')[0].reset();
                    $('.statusMsg').html('<p class="alert alert-success">'+response.message+'</p>');
                }else{
                    $('.statusMsg').html('<p class="alert alert-danger">'+response.message+'</p>');
                }
                $('#newAppForm').css("opacity","");
                $(".submitBtn").removeAttr("disabled");
            },
            error: function(req, err) {
                $('.statusMsg').html('<p class="alert alert-danger">An unexpected error has occurred. Please try again later or contact miro@gams.com if this error persists.</p>');
            }
        });
    });

    /* ===== Stickyfill ===== */
    /* Ref: https://github.com/wilddeer/stickyfill */
    // Add browser support to position: sticky
    var elements = $('.sticky');
    Stickyfill.add(elements);


    /* Hack related to: https://github.com/twbs/bootstrap/issues/10236 */
    $(window).on('load resize', function() {
        $(window).trigger('scroll');
    });

    /* Activate scrollspy menu */
    $('body').scrollspy({target: '#doc-menu', offset: 100});

    /* Smooth scrolling */
    $('a.scrollto').on('click', function(e){
        //store hash
        var target = this.hash;
        e.preventDefault();
        $('body').scrollTo(target, 800, {offset: 0, 'axis':'y'});

    });


    /* ======= jQuery Responsive equal heights plugin ======= */
    /* Ref: https://github.com/liabru/jquery-match-height */

    // $('#cards-wrapper .item-inner').matchHeight();
    // $('#showcase .card').matchHeight();

    /* Bootstrap lightbox */
    /* Ref: http://ashleydw.github.io/lightbox/ */

    $(document).delegate('*[data-toggle="lightbox"]', 'click', function(e) {
        e.preventDefault();
        $(this).ekkoLightbox();
    });
});
(function ($) {
    /**
     * jquery-match-height master by @liabru
     * http://brm.io/jquery-match-height/
     * License: MIT
     */

    ;(function(factory) { // eslint-disable-line no-extra-semi
        'use strict';
        if (typeof define === 'function' && define.amd) {
            // AMD
            define(['jquery'], factory);
        } else if (typeof module !== 'undefined' && module.exports) {
            // CommonJS
            module.exports = factory(require('jquery'));
        } else {
            // Global
            factory(jQuery);
        }
    })(function($) {
        /*
         *  internal
         */

        var _previousResizeWidth = -1,
            _updateTimeout = -1;

        /*
         *  _parse
         *  value parse utility function
         */

        var _parse = function(value) {
            // parse value and convert NaN to 0
            return parseFloat(value) || 0;
        };

        /*
         *  _rows
         *  utility function returns array of jQuery selections representing each row
         *  (as displayed after float wrapping applied by browser)
         */

        var _rows = function(elements) {
            var tolerance = 1,
                $elements = $(elements),
                lastTop = null,
                rows = [];

            // group elements by their top position
            $elements.each(function(){
                var $that = $(this),
                    top = $that.offset().top - _parse($that.css('margin-top')),
                    lastRow = rows.length > 0 ? rows[rows.length - 1] : null;

                if (lastRow === null) {
                    // first item on the row, so just push it
                    rows.push($that);
                } else {
                    // if the row top is the same, add to the row group
                    if (Math.floor(Math.abs(lastTop - top)) <= tolerance) {
                        rows[rows.length - 1] = lastRow.add($that);
                    } else {
                        // otherwise start a new row group
                        rows.push($that);
                    }
                }

                // keep track of the last row top
                lastTop = top;
            });

            return rows;
        };

        /*
         *  _parseOptions
         *  handle plugin options
         */

        var _parseOptions = function(options) {
            var opts = {
                byRow: true,
                property: 'height',
                target: null,
                remove: false
            };

            if (typeof options === 'object') {
                return $.extend(opts, options);
            }

            if (typeof options === 'boolean') {
                opts.byRow = options;
            } else if (options === 'remove') {
                opts.remove = true;
            }

            return opts;
        };

        /*
         *  matchHeight
         *  plugin definition
         */

        var matchHeight = $.fn.matchHeight = function(options) {
            var opts = _parseOptions(options);

            // handle remove
            if (opts.remove) {
                var that = this;

                // remove fixed height from all selected elements
                this.css(opts.property, '');

                // remove selected elements from all groups
                $.each(matchHeight._groups, function(key, group) {
                    group.elements = group.elements.not(that);
                });

                // TODO: cleanup empty groups

                return this;
            }

            if (this.length <= 1 && !opts.target) {
                return this;
            }

            // keep track of this group so we can re-apply later on load and resize events
            matchHeight._groups.push({
                elements: this,
                options: opts
            });

            // match each element's height to the tallest element in the selection
            matchHeight._apply(this, opts);

            return this;
        };

        /*
         *  plugin global options
         */

        matchHeight.version = 'master';
        matchHeight._groups = [];
        matchHeight._throttle = 80;
        matchHeight._maintainScroll = false;
        matchHeight._beforeUpdate = null;
        matchHeight._afterUpdate = null;
        matchHeight._rows = _rows;
        matchHeight._parse = _parse;
        matchHeight._parseOptions = _parseOptions;

        /*
         *  matchHeight._apply
         *  apply matchHeight to given elements
         */

        matchHeight._apply = function(elements, options) {
            var opts = _parseOptions(options),
                $elements = $(elements),
                rows = [$elements];

            // take note of scroll position
            var scrollTop = $(window).scrollTop(),
                htmlHeight = $('html').outerHeight(true);

            // get hidden parents
            var $hiddenParents = $elements.parents().filter(':hidden');

            // cache the original inline style
            $hiddenParents.each(function() {
                var $that = $(this);
                $that.data('style-cache', $that.attr('style'));
            });

            // temporarily must force hidden parents visible
            $hiddenParents.css('display', 'block');

            // get rows if using byRow, otherwise assume one row
            if (opts.byRow && !opts.target) {

                // must first force an arbitrary equal height so floating elements break evenly
                $elements.each(function() {
                    var $that = $(this),
                        display = $that.css('display');

                    // temporarily force a usable display value
                    if (display !== 'inline-block' && display !== 'flex' && display !== 'inline-flex') {
                        display = 'block';
                    }

                    // cache the original inline style
                    $that.data('style-cache', $that.attr('style'));

                    $that.css({
                        'display': display,
                        'padding-top': '0',
                        'padding-bottom': '0',
                        'margin-top': '0',
                        'margin-bottom': '0',
                        'border-top-width': '0',
                        'border-bottom-width': '0',
                        'height': '100px',
                        'overflow': 'hidden'
                    });
                });

                // get the array of rows (based on element top position)
                rows = _rows($elements);

                // revert original inline styles
                $elements.each(function() {
                    var $that = $(this);
                    $that.attr('style', $that.data('style-cache') || '');
                });
            }

            $.each(rows, function(key, row) {
                var $row = $(row),
                    targetHeight = 0;

                if (!opts.target) {
                    // skip apply to rows with only one item
                    if (opts.byRow && $row.length <= 1) {
                        $row.css(opts.property, '');
                        return;
                    }

                    // iterate the row and find the max height
                    $row.each(function(){
                        var $that = $(this),
                            style = $that.attr('style'),
                            display = $that.css('display');

                        // temporarily force a usable display value
                        if (display !== 'inline-block' && display !== 'flex' && display !== 'inline-flex') {
                            display = 'block';
                        }

                        // ensure we get the correct actual height (and not a previously set height value)
                        var css = { 'display': display };
                        css[opts.property] = '';
                        $that.css(css);

                        // find the max height (including padding, but not margin)
                        if ($that.outerHeight(false) > targetHeight) {
                            targetHeight = $that.outerHeight(false);
                        }

                        // revert styles
                        if (style) {
                            $that.attr('style', style);
                        } else {
                            $that.css('display', '');
                        }
                    });
                } else {
                    // if target set, use the height of the target element
                    targetHeight = opts.target.outerHeight(false);
                }

                // iterate the row and apply the height to all elements
                $row.each(function(){
                    var $that = $(this),
                        verticalPadding = 0;

                    // don't apply to a target
                    if (opts.target && $that.is(opts.target)) {
                        return;
                    }

                    // handle padding and border correctly (required when not using border-box)
                    if ($that.css('box-sizing') !== 'border-box') {
                        verticalPadding += _parse($that.css('border-top-width')) + _parse($that.css('border-bottom-width'));
                        verticalPadding += _parse($that.css('padding-top')) + _parse($that.css('padding-bottom'));
                    }

                    // set the height (accounting for padding and border)
                    $that.css(opts.property, (targetHeight - verticalPadding) + 'px');
                });
            });

            // revert hidden parents
            $hiddenParents.each(function() {
                var $that = $(this);
                $that.attr('style', $that.data('style-cache') || null);
            });

            // restore scroll position if enabled
            if (matchHeight._maintainScroll) {
                $(window).scrollTop((scrollTop / htmlHeight) * $('html').outerHeight(true));
            }

            return this;
        };

        /*
         *  matchHeight._applyDataApi
         *  applies matchHeight to all elements with a data-match-height attribute
         */

        matchHeight._applyDataApi = function() {
            var groups = {};

            // generate groups by their groupId set by elements using data-match-height
            $('[data-match-height], [data-mh]').each(function() {
                var $this = $(this),
                    groupId = $this.attr('data-mh') || $this.attr('data-match-height');

                if (groupId in groups) {
                    groups[groupId] = groups[groupId].add($this);
                } else {
                    groups[groupId] = $this;
                }
            });

            // apply matchHeight to each group
            $.each(groups, function() {
                this.matchHeight(true);
            });
        };

        /*
         *  matchHeight._update
         *  updates matchHeight on all current groups with their correct options
         */

        var _update = function(event) {
            if (matchHeight._beforeUpdate) {
                matchHeight._beforeUpdate(event, matchHeight._groups);
            }

            $.each(matchHeight._groups, function() {
                matchHeight._apply(this.elements, this.options);
            });

            if (matchHeight._afterUpdate) {
                matchHeight._afterUpdate(event, matchHeight._groups);
            }
        };

        matchHeight._update = function(throttle, event) {
            // prevent update if fired from a resize event
            // where the viewport width hasn't actually changed
            // fixes an event looping bug in IE8
            if (event && event.type === 'resize') {
                var windowWidth = $(window).width();
                if (windowWidth === _previousResizeWidth) {
                    return;
                }
                _previousResizeWidth = windowWidth;
            }

            // throttle updates
            if (!throttle) {
                _update(event);
            } else if (_updateTimeout === -1) {
                _updateTimeout = setTimeout(function() {
                    _update(event);
                    _updateTimeout = -1;
                }, matchHeight._throttle);
            }
        };

        /*
         *  bind events
         */

        // apply on DOM ready event
        $(matchHeight._applyDataApi);

        // use on or bind where supported
        var on = $.fn.on ? 'on' : 'bind';

        // update heights on load and resize events
        $(window)[on]('load', function(event) {
            matchHeight._update(false, event);
        });

        // throttled update heights on resize events
        $(window)[on]('resize orientationchange', function(event) {
            matchHeight._update(true, event);
        });

    });

    $( document ).ready(function($) {
        var queryToMark = new URL(window.location.href).searchParams.get("search");
        if (queryToMark) {
            var bodyToMark = $(".doc-body");
            if (!$(".doc-body").length) {
                bodyToMark = $(".index-page");
            }
            bodyToMark.mark(queryToMark, {
                accuracy: "exactly",
                done: function() {
                $("body").scrollTo("mark");
            }});
        }
        // credits to: bjornd: https://stackoverflow.com/questions/6234773/can-i-escape-html-special-chars-in-javascript

          function escapeHtml(unsafe) {
            return unsafe
                 .replace(/&/g, "&amp;")
                 .replace(/</g, "&lt;")
                 .replace(/>/g, "&gt;")
                 .replace(/"/g, "&quot;")
                 .replace(/'/g, "&#039;");
          }
          var searchRequest;
          var $searchResultsBox = $("#miroSearchResults");
          $("#miroSearch").on("keyup", $.debounce( 250, function(e) {
            if (searchRequest) {
                searchRequest.abort();
            }
            var searchTerm = e.target.value;
            if (searchTerm.length > 0) {
                $searchResultsBox.html('<li class="list-group-item"><i>Searching...</i></li>');
                var encodedSearchTerm = encodeURIComponent(searchTerm);
                searchRequest = $.getJSON( "https://search.gams.com/miro/select?q="+
                    encodedSearchTerm + "&df=content&rows=5&hl=true&hl.snippets=2&hl.fl=content&hl.fragsize=200&hl.q=" + encodedSearchTerm + "&fl=url,title&q.op=AND&indent=on&defType=edismax",
                    function( data ) {
                        if (data.response.docs.length) {
                            $searchResultsBox.empty();
                            for (var i=0; i < data.response.docs.length; i++) {
                                var resultUrl = data.response.docs[i].url;
                                var content = data.highlighting[resultUrl].content;
                                if (content == null) {
                                    content = "";
                                }
                                $searchResultsBox.append('<a class="list-group-item list-group-item-action" href="' +
                                    resultUrl + '?search=' + encodedSearchTerm + '"><b>'+
                                    escapeHtml(data.response.docs[i].title) + '</b><br>' + content + '</a>');
                            }
                        } else {
                            $searchResultsBox.html('<li class="list-group-item"><i>No results</i></li>');
                        }
                        $searchResultsBox.show();
                })
                .fail(function(jqxhr, textStatus, error) {
                    console.log( "Request Failed: " + textStatus + ", " + error );
                    $searchResultsBox.html('<li class="list-group-item text-danger"><b>A problem has occurred. Please try again later.</b></li>');
                    $searchResultsBox.show();
                })
            } else {
                $searchResultsBox.empty();
            }
          }));
          $("#miroSearch").on("focus", function(e) {
            $("#miroSearch").animate({width: 300, paddingRight: ''}, 200);
          });
          $("#miroSearch").on("blur", function(e) {
            setTimeout(function() {
                $searchResultsBox.fadeOut(100);
                $("#miroSearch").animate({width: 30, paddingRight: 0}, 200);
                $("#miroSearch").val("");
            }, 100);
          });
    });
})(jQuery);
