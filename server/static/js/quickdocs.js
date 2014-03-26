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

Quickdocs.breadcrumbTimer_ = undefined;
Quickdocs.updateBreadcrumbHeader = function() {
    var el = Quickdocs.getCurrentScrollEl('.system');
    if (el) {
        var $el = $(el);
        var breadcrumb = $('.breadcrumb-header-container');
        breadcrumb.children('.system-name, .package-name').remove();

        var currentSystem = $el.find('header h1').text();
        var systemLink = $('<li class="system-name">')
                .append($('<a href="#system-' + currentSystem + '">')
                        .text(currentSystem));
        breadcrumb.append(systemLink);

        var packageEl = Quickdocs.getCurrentScrollEl($el.find('.package'));
        if (packageEl) {
            var currentPackage = $(packageEl).find('h2').text();

            var packageLink = $('<li class="package-name">')
                    .append($('<a href="#package-' + currentPackage + '">')
                            .text(currentPackage));
            packageLink.addClass('current');
            breadcrumb.append(packageLink);
        }
        else {
            systemLink.addClass('current');
        }

        $('.breadcrumb-header-container').show().css({ opacity: 0.7 });
        $('.pager-link-container').show();

        if (Quickdocs.breadcrumbTimer_ !== undefined) {
            clearTimeout(Quickdocs.breadcrumbTimer_);
        }
        Quickdocs.breadcrumbTimer_ = setTimeout(function() {
            $('.breadcrumb-header-container').css({ opacity: 0.5 }, 100);
            Quickdocs.breadcrumbTimer_ = undefined;
        }, 200);
    }
    else {
        $('.breadcrumb-header-container').hide();
        $('.pager-link-container').hide();
    }
};

$(document).on('scroll', Quickdocs.updateBreadcrumbHeader);

Quickdocs.smoothScrollTo = function(el, opt_speed) {
    var speed = opt_speed || 300;
    var $el = $(el);
    if ($el.length !== 0) {
        var position = $el.offset().top;
        $(/safari/i.test(navigator.userAgent) ? 'body' : 'html').animate({
            scrollTop: position
        }, speed, 'swing', function() {
            var id =  $el.attr('id');
            if (!id) {
                id = $el.find('h1,h2').attr('id');
            }
            if (id && window.history && window.history.pushState) {
                history.pushState({}, null, '#'+id);
            }
        });
    }
};

$(document).on('click', 'a[href^=#]', function(e) {
    e.preventDefault();
    var href = $(this).attr("href");
    href = href.replace(/\./g, '\\.');
    var target = $(href == "#" || href == "" ? 'html' : href);
    Quickdocs.smoothScrollTo(target);
});

$(document).on('click', '.error-open-detail', function(e) {
    var detail = $(this).hide().closest('.error').find('.error-detail');

    var height = detail.height();
    detail.css({
        height: 0
    }).show().animate({'height': height}, 200);
});

Quickdocs.getPrevComponent = function(el) {
    var $el = $(el);
    if ($el.hasClass('package')) {
        var prev = $el.prev('.package');
        if (prev.length === 0) {
            return $el.closest('.system');
        }
        return prev;
    }
    else if ($el.hasClass('system')) {
        var prev = $el.prev('.system');
        if (prev.length === 0) {
            return $('#global-header');
        }
        return prev;
    }
    return $el;
};

Quickdocs.getNextComponent = function(el) {
    var $el = $(el);
    if ($el.hasClass('package')) {
        var next = $el.next('.package');
        if (next.length === 0) {
            next = $el.closest('.system').next('.system');
            if (next.length === 0) {
                return $('#global-footer');
            }
        }
        return next;
    }
    else if ($el.hasClass('system')) {
        var next = $el.find('.package').first();
        if (next.length === 0) {
            return $('#global-footer');
        }
        return next;
    }
    return $el;
};

$(document).on('click', '.pager-link', function(e) {
    var target = $(e.target);
    var current = $('.breadcrumb-header-container .current a').attr('href');
    current = current.replace(/\./g, '\\.');
    if (target.attr('title') === 'up') {
        var scrollTo = Quickdocs.getPrevComponent($(current).closest('.package, .system'));
        Quickdocs.smoothScrollTo(scrollTo);
    }
    else if (target.attr('title') === 'down') {
        var scrollTo = Quickdocs.getNextComponent($(current).closest('.package, .system'));
        Quickdocs.smoothScrollTo(scrollTo);
    }
});

Quickdocs.init.done(function() {
$('.folding-list').each(function(i, list) {
    var rows = $(list).children('li');
    if (rows.length > 3) {
        rows.slice(3).hide();
        var loadMore = $('<a class="lsf">down</a>').on('click', function() {
            var parent = $(this).closest('li');
            parent.siblings('li:hidden').fadeIn();
            parent.hide();
        });
        $('<li class="load-more">').append(loadMore).appendTo(list);
    }
});
});

}).call(this, jQuery, Quickdocs);