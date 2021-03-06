/*
	Spatial by TEMPLATED
	templated.co @templatedco
	Released for free under the Creative Commons Attribution 3.0 license (templated.co/license)
*/

(function($) {

	skel.init({
		reset: 'full',
		breakpoints: {
			global: {
				href: '/assets/css/style.css',
				containers: 1400,
				grid: { gutters: ['2em', 0] }
			},
			xlarge: {
				media: '(max-width: 1680px)',
				href: '/assets/css/style-xlarge.css',
				containers: 1200
			},
			large: {
				media: '(max-width: 1280px)',
				href: '/assets/css/style-large.css',
				containers: 960,
				grid: { gutters: ['1.5em', 0] },
				viewport: { scalable: false }
			},
			medium: {
				media: '(max-width: 980px)',
				href: '/assets/css/style-medium.css',
				containers: '90%!'
			},
			small: {
				media: '(max-width: 736px)',
				href: '/assets/css/style-small.css',
				containers: '90%!',
				grid: { gutters: ['1.25em', 0] }
			},
			xsmall: {
				media: '(max-width: 480px)',
				href: '/assets/css/style-xsmall.css'
			}
		},
		plugins: {
			layers: {
				navPanel: {
					animation: 'pushX',
					breakpoints: 'medium',
					clickToHide: true,
					height: '100%',
					hidden: true,
					html: '<div data-action="moveElement" data-args="nav"></div>',
					orientation: 'vertical',
					position: 'top-left',
					side: 'left',
					width: 250
				},
				navButton: {
					breakpoints: 'medium',
					height: '4em',
					html: '<span class="toggle" data-action="toggleLayer" data-args="navPanel"></span>',
					position: 'top-left',
					side: 'top',
					width: '6em'
				}
			}
		}
	});

	$(function() {

		var	$window = $(window),
			$body = $('body');

		// Disable animations/transitions until the page has loaded.
			$body.addClass('is-loading');

			$window.on('load', function() {
				$body.removeClass('is-loading');
			});

		// Touch mode.
			if (skel.vars.isMobile)
				$body.addClass('is-touch');

	});

})(jQuery);

/* Text Animation */
$(function () {
    $('.tlt').textillate({
        minDisplayTime: 2000,
        initialDelay: 0,
        in: {
            effect: 'fadeIn',
            delayScale: 1.5,
            delay:100,
            shuffle: true,
        },

        out: {
            effect: 'fadeOut',
            delayScale: 1.5,
            delay: 50,
            shuffle: true
        },

        type: 'char'
    });
});

$(function () {
    $('.tlt2').textillate({
        minDisplayTime: 2000,
        initialDelay: 1600,
        in: {
            effect: 'fadeIn',
            delayScale: 1.5,
            delay:100,
            shuffle: true
        },

        out: {
            effect: 'fadeOut',
            delayScale: 1.5,
            delay: 50,
            shuffle: true,
        },

        type: 'char'
    });
});

$(function () {
    $('.nametag').textillate({
        minDisplayTime: 2000,
        initialDelay: 2700,
        in: {
            effect: 'fadeInLeft',
            delayScale: 1.5,
            delay: 150,
            reverse: true
        },

        out: {
            effect: 'fadeOut',
            delayScale: 1.5,
            delay: 50,
            shuffle: true,
        },

        type: 'char'
    });
});
/* Page Scroll */
$("#scroller").click(function() {
    $('html, body').animate({
        scrollTop: $("#about").offset().top
    }, 2000);
});
