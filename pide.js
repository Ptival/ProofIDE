
function haskell(f, s) {
    var o = { x : s };
    f(o);
    return o.y;
}

function hsParseTerm(s) { return haskell(window.parseTerm, s); }

function hsTypeCheck(s) { return haskell(window.typeCheck, s); }

function hsTypeCheckDebug(s) { return haskell(window.typeCheckDebug, s); }

// This is used to compute rendered sizes
hiddenDiv = null;

pideCustomElements =
[ 'pide-block'
, 'pide-syntax'
, 'pide-match'
, 'pide-subexp'
, 'pide-with'
, 'pide-patterns'
, 'pide-case'
, 'pide-pattern'
, 'pide-casebody'
, 'pide-end'
];

function mkBlock() { return document.createElement('pide-block'); }

function mkSubexp() { return document.createElement('pide-subexp'); }

function syntax(text) {
    e = document.createElement('pide-syntax');
    e.appendChild(document.createTextNode(text));
    return e;
}

function isPi(e) {
    e.tag && e.tag  == 'Pi';
}

function htmlize(root, j){
    const t = j.tag;
    const c = j.contents;
    /* the generic JSONizer doesn't put contents of single-parameter constructors
       in a 1-size array, but leaves them plain, so we need to take care of the
       disparity here... */
    const annotation = (c instanceof Array && c.length > 0) ? c[0] : c;
    if(annotation.Right) {
        var typeroot = mkSubexp();

        var context = '';
        if(t == 'Hole') {
            _(annotation.Right[0]).map(function(e){
                var ctxttyperoot = mkSubexp();
                htmlize(ctxttyperoot, e[1]);
                context = context + e[0];
                context = context + ' : ' + $(ctxttyperoot).text();
                context = context + '<br/>';
            });
        }

        if (context != '') {
            context = context + '========================================<br/>';
        }

        htmlize(typeroot, annotation.Right[1]);
        $(root)
            .attr('title', context + typeroot.innerHTML)
            .tooltip()
        ;
    }
    else if(annotation.Left) {
        $(root)
            .attr('title', annotation.Left.replace(/\n/g, '<br>'))
            .addClass('illtyped')
            .tooltip()
        ;
    }
    else {
        $(root)
            .attr('title', 'Type')
            .tooltip()
        ;
    }

    switch (t) {
        case 'Type':
        e = document.createElement('pide-syntax');
        e.innerHTML = "Type";
        root.appendChild(e);
        break;

        case 'Pi':
        if(c[1]) {
            root.appendChild(syntax('('));
            root.appendChild(syntax(c[1]));
            root.appendChild(syntax('\u00a0:\u00a0'));
        }
        if(!c[1] && c[2].tag == 'Pi') { root.appendChild(syntax('(')); }
        root.appendChild(htmlize(mkSubexp(), c[2]));
        if(!c[1] && c[2].tag == 'Pi') { root.appendChild(syntax(')')); }
        if(c[1]) { root.appendChild(syntax(')')); }
        root.appendChild(syntax('\u00a0→\u00a0'));
        root.appendChild(htmlize(mkSubexp(), c[3]));
        break;

        case 'Var':
        root.appendChild(document.createTextNode(c[1]));
        break;

        case 'Lam':
        root.appendChild(syntax('λ'));
        if(c[1]) { root.appendChild(syntax(c[1])); }
        else { root.appendChild(syntax('_')); }
        root.appendChild(syntax('\u00a0'));
        root.appendChild(htmlize(mkSubexp(), c[2]));
        break;

        case 'App':
        var left = htmlize(mkSubexp(), c[1]);
        if(c[1].tag == 'Lam') {
            $(left).prepend(syntax('('));
            $(left).append(syntax(')'));
        }
        root.appendChild(left);
        root.appendChild(syntax('\u00a0'));
        right = htmlize(mkSubexp(), c[2]);
        if(c[2].tag == 'App' || c[2].tag == 'Lam') {
            $(right).prepend(syntax('('));
            $(right).append(syntax(')'));
        }
        root.appendChild(right);
        break;

        case 'Hole':
        var hole = document.createElement('textarea');
        $(hole).each(resizeTextarea);
        hole.innerHTML = '';
        root.appendChild(hole);
        break;

        case 'Annot':
        root.appendChild(htmlize(mkSubexp(), c[1]));
        root.appendChild(syntax('\u00a0@\u00a0'));
        root.appendChild(htmlize(mkSubexp(), c[2]));
        break;

        default:
        alert('Unhandled tag: ' + t);
    };
    return root;
}

function resizeTextarea(){
    content = $(this).val();
    hiddenDiv.html(content.replace(/\n/g, '&nbsp;&nbsp;<br>').replace(/ /g, '&nbsp;') + '&nbsp;&nbsp;<br>');
    $(this).css('width', Math.max(hiddenDiv.width(), 10));
    $(this).css('height', hiddenDiv.height());
}

function addText(t) {
    textBlock = mkBlock();
    textNode = document.createTextNode(t);
    textBlock.appendChild(textNode);
    $(textBlock).insertBefore($('body > .last'));
}

function addTerm(t) {
    var textBlock = mkBlock();
    var subexpBlock = mkSubexp();
    var textArea = $('<textarea/>');
    textArea.text(t);
    $(textArea).each(resizeTextarea);
    $(subexpBlock).append(textArea);
    textBlock.appendChild(subexpBlock);
    addCheckButton(textBlock);
    $(textBlock).insertBefore($('body > .last'));
}

function textify(block) {
    $(block).find('textarea').replaceWith(function() {
        var contents = $(this).val();

        // In the absence of contents, we make a hole
        var t = '?';

        if(contents) {
            // parenthesize if the expression ought to be parenthesized

            /*
              if the expression is (foo : T), we don't want to add parentheses,
              since it would throw off the parser upon seeing:
              ((foo : T)) → X
            */
            var re = /^\([a-zA-Z][a-zA-Z0-9]*\s*:.*\)$/g

            if(re.test(contents)) {
                t = contents;
            } else {
                t = '(' + contents + ')';
            }
        }

        return t;
    });
    return $(block).text();
}

function addCheckButton(e) {
    var button = document.createElement('input');
    $(button)
        .attr('type', 'button')
        .attr('value', 'Check')
        .click(function() {
            var block = $(this).closest('pide-block');
            var program = textify(block, true);
            var t = hsTypeCheck(program);
            //alert(t);
            var res = jQuery.parseJSON(unescape(t));
            var root = mkSubexp();
            htmlize(root, res);
            block.children('pide-subexp').remove();
            block.prepend(root);
        })
    ;
    $(e).append(button);
}

function display(t) {
    var block = mkBlock();
    var root = mkSubexp();
    htmlize(root, t);
    addCheckButton(block);
    block.appendChild(root);
    $('body').append(block);
}

window.onload = function (){

    hiddenDiv = $('#pide-invisible');

    _(pideCustomElements).each(function(e){
        xtag.register(e, {});
    });

    $(document)
        .on('focusin', 'textarea', function(e){
            $(this).css('background-color', 'lightblue');
        })
        .on('focusout', 'textarea', function(e){
            $(this).css('background-color', 'white');
        })
        .on('change keyup keydown paste', 'textarea', resizeTextarea)
    ;

    $(document)
        .on('mouseenter mouseover', 'pide-subexp', function(e){
            $(this).css('background-color',
                        $(this).hasClass('illtyped') ? 'crimson' : 'lightblue');
            e.stopImmediatePropagation();
        })
        .on('mouseout mouseleave', 'pide-subexp', function(e){
            $(this).css('background-color', 'transparent');
            e.stopImmediatePropagation();
        })
    ;

    $(document)
        .on('dblclick', 'pide-subexp', function(e){
            e.stopImmediatePropagation();

            // only turn into textarea if it isn't one yet
            if($(this).children().length == 1
               && $(this).children('textarea').length == 1)
                return;

            var txt = textify($(this), false);
            $(this).empty();
            var hole = document.createElement('textarea');
            hole.innerHTML = txt;
            $(this).append(hole);
            $(hole).each(resizeTextarea);

            $(hole).focus();

        })
    ;

    $(document)
        .on('click', '#pide-add', function(){
            addTerm('? @ ?');
        })
    ;

    $.widget("ui.tooltip", $.ui.tooltip, {
        options: {
            content: function () {
                return $(this).prop('title');
            }
        }
    });

    onHaskellReady();
}

function onHaskellReady() {
    if (window.haskellReady) {
        fillPage();
    }
    else
    {
        window.setTimeout("onHaskellReady();", 100);
    }
}

function fillPage() {
    addText('A well-typed flip:');
    addTerm('λA λB λC λf λb λa f a b @ (A : Type) → (B : Type) → (C : Type) → (A → B → C) → B → A → C');

    addText('An ill-typed flip:');
    addTerm('λA λB λC λf λb λa f a a @ (A : Type) → (B : Type) → (C : Type) → (A → B → C) → B → A → C');

    addText('A partially-implemented flip:');
    addTerm('λA λB λC λf λb λa ? @ (A : Type) → (B : Type) → (C : Type) → (A → B → C) → B → A → C');

    $('pide-subexp').each(function() { $(this).tooltip(); });

    $('textarea').each(resizeTextarea);
}
