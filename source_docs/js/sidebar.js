(function($) {
    $(function() {
        if (typeof cssmenu_no_js === 'undefined') {
            // highlight menu entry of the current page
            var href = window.location.href.split('#')[0];
            var current;
            var res = $('.sidebar a').each(function (_, a) {
                if (a.href == href) {
                    current = a;
                    return false;
                }
            });
            current = $(current);
            console.log(current);
            // direct li parent containing the link
            current.parent('li').addClass('active');
            // topmost li parent, e.g. 'std'
            current.parents('.sidebar .expand-container').addClass('open');

            var open_main_item = null;
            $('.expand-toggle').click(function(e) {
                var container = $(this).parent('.expand-container');
                container.toggleClass('open');
                return false;
            });
        }
    });
})(jQuery);
