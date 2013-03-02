var Quickdocs = Quickdocs || {};

(function($, Quickdocs) {

if (Quickdocs.init) return;

Quickdocs.init = $.Deferred(function() { $(this.resolve); }).promise();

Quickdocs.getCurrentScrollEl = function(jqObj) {
    var scrollTop = $(document).scrollTop();
    return _.chain($(jqObj))
            .filter(function(el) { return scrollTop > $(el).position().top; })
            .last()
            .value();
};

Quickdocs.updateBreadcrumbHeader = function() {
    var el = Quickdocs.getCurrentScrollEl('.system');
    if (el) {
        var $el = $(el);
        $('.breadcrumb-header').text('');

        var currentSystem = $el.find('header h1').text();
        var systemLink = $('<span class="system-name">')
                .append($('<a href="#system-' + currentSystem + '">')
                        .text(currentSystem));
        $('.breadcrumb-header').append(systemLink);

        var packageEl = Quickdocs.getCurrentScrollEl($el.children('.package'));
        if (packageEl) {
            var currentPackage = $(packageEl).find('h2').text();

            var packageLink = $('<span class="package-name">')
                    .append($('<a href="#package-' + currentPackage + '">')
                            .text(currentPackage));
            packageLink.addClass('current');
            $('.breadcrumb-header').append(packageLink);
        }
        else {
            systemLink.addClass('current');
        }

        $('.breadcrumb-header-container').show();
    }
    else {
        $('.breadcrumb-header-container').hide();
    }
};

$(document).on('scroll', Quickdocs.updateBreadcrumbHeader);

$(document).on('click', 'a[href^=#]', function(e) {
    var speed = 300;
    var href = $(this).attr("href");
    href = href.replace('.', '\\.');
    var target = $(href == "#" || href == "" ? 'html' : href);
    if (target) {
        var position = target.offset().top;
        $(/safari/i.test(navigator.userAgent) ? 'body' : 'html').animate({
            scrollTop: position
        }, speed, 'swing');
        return false;
    }
    return true;
});

}).call(this, jQuery, Quickdocs);