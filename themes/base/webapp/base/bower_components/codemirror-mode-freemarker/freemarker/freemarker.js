// CodeMirror, copyright (c) by Liuxiaole
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"), require("../htmlmixed/htmlmixed"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror", "../htmlmixed/htmlmixed"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  "use strict";

  var prevContent;

  var identifier = /[0-9a-zA-Z-_$]/;
  var isOperatorChar = /[+\-*\/%=<>!\?.&:,;]/;

  var pairCloseDirective = " if switch macro function list noparse compress escape noescape attempt recurse fallback ";

  var selfCloseDirective = " else elseif case default break return nested assign include import global local setting flush stop ftl t lt rt nt recover visit recure ";

  var builtin = "substring cap_first uncap_first capitalize chop_linebreak date time datetime ends_with html group index_of j_string js_string js_string length lower_case left_pad right_pad contains matches number replace rtf url split starts_with string trim upper_case word_list xhtml xml c round floor ceiling short medium long full first last seq_contains seq_index_of seq_last_index_of reverse size sort sort_by chunk keys byte double float int eval has_content interpret is_string is_number is_boolean is_date is_method is_transform is_macro is_hash is_hash_ex is_sequence is_collection is_enumerable is_indexable is_directive is_node namespace new";

  var atoms = "as in true false using gt gte lt lte";

  function words(str) {
    var obj = {}, words = str.split(/\s+/);
    for (var i = 0; i < words.length; ++i) obj[words[i]] = true;
    return obj;
  }

  CodeMirror.defineMode("ftl:inner", function(config, parserConfig){

    function Context(indented, column, type, align, prev) {
      this.indented = indented;
      this.column = column;
      this.type = type;
      this.align = align;
      this.prev = prev;
    }

    function pushContext(state, col, type, align) {
      return state.context = new Context(state.indented, col, type, align, state.context);
    }

    function popContext(state) {
      if (!state.context.prev) return;
      var t = state.context.type;
      state.indented = state.context.indented;

      return state.context = state.context.prev;
    }

    function tokenComment(stream, state) {
      while (!stream.eol()) {
        if (stream.match('-->')) {
          state.tokenize = null;
          break;
        }
        stream.next();
      }
      return "comment";
    }

    function tokenBase(end, style, hook){
      function tokenBaseInner(stream, state){
        var ch = stream.next();

        if (ch == '"' || ch == "'") {
          var curToken = state.tokenize;
          state.tokenize = tokenString(ch, curToken);
          return state.tokenize(stream, state);
        }
        if (/\d/.test(ch)) {
          stream.match(/^[0-9]*\.?[0-9]*([eE][\-+]?[0-9]+)?/);
          return "number";
        }

        if (/[\[\]{}\(\)]/.test(ch)) {
          if(ch == '{') pushContext(state, stream.column(), '}');
          else if(ch == '[') pushContext(state, stream.column(), ']');
          else if(ch == '(') pushContext(state, stream.column(), ')');
          else {
            if(ch === state.context.type) {
              popContext(state);
              return 'bracket';
            } else if(ch === end) {
              state.tokenize = null;
              return style;
            }
          }

          return 'bracket';
        }

        if (isOperatorChar.test(ch)) {
          if(ch == '>' && end == '>') {
            state.tokenize = null;
            return style;
          }


          if(ch == '&') {
            stream.match(/gt;|lt;/);
          } 

          if(ch == '/' && end == '>' && stream.peek() == end) {
            stream.next();
            state.tokenize = null;
            return style;
          }

          return "operator";
        }

        stream.eatWhile(identifier);
        var cur = stream.current();

        if (prevContent == '?' && parserConfig.builtin.propertyIsEnumerable(cur)) {
          return "builtin";
        }

        if (parserConfig.atoms.propertyIsEnumerable(cur)) return "atom";

        return "variable";
      };

      return function(stream, state){
        var style = tokenBaseInner(stream, state);
        var content = stream.current();

        if(typeof hook === 'function') {
          style = hook(style, content, stream, state);
        }

        return style;
      };
    }


    function tokenString(quote, endToken) {
      return function(stream, state) {
        var escaped = false, next, end = false;
        while ((next = stream.next()) != null) {
          if (next == quote && !escaped) {end = true; break;}
          escaped = !escaped && next == "\\";
        }
        if (end) state.tokenize = endToken;
        return "string";
      };
    }


    function tokenOpenDirective(stream, state){
      stream.eatWhile(identifier);
      var directive = stream.current();
      var style;

      if(parserConfig.directive.propertyIsEnumerable(directive)) {
        style = "keyword";
      } else {
        style = 'keyword error';
      }


      pushContext(state, stream.column(), directive);

      var hook, isFirst;
      if(directive == 'function' || directive == 'macro') {
        isFirst = true;
        hook = function(style, content, stream, state){
          if(isFirst) {
            isFirst = false;
            if(style != 'variable') return 'error';
            else return 'def';
          }

          return style;
        };
      } 

      state.tokenize = tokenBase('>', 'tag bracket', hook);


      return style;
    }

    function tokenCloseDirective(stream, state){
      if(stream.eat('>')) {
        state.tokenize = null;
        return 'tag bracket';
      }

      stream.eatWhile(/[^\>]/);
      
      var directive = stream.current();
      var style;

      if(parserConfig.directive.propertyIsEnumerable(directive)) {
        style = 'keyword';
        
        while(directive != state.context.type && 
            parserConfig.selfCloseDirective.propertyIsEnumerable(state.context.type) ) {
          popContext(state);
        }

        if(directive == state.context.type) {
          popContext(state);
        } else {
          style += ' error';
        }

      } else {
        style = 'error'
      }

      return style;
    }


    function tokenOpenMacro(stream, state){
      var isFirst = true, macroName;
      var hook = function(style, content, stream, state){
        if(isFirst && style == 'variable') {
          isFirst = false;
          macroName = content;
          return 'variable-3';
        }
        if(style == 'tag bracket' && content == '>') {
          pushContext(state, stream.column(), '@'+macroName);
        }
        return style;
      };

      state.tokenize = tokenBase('>','tag bracket',hook);
      return 'tag bracket';
    }


    function tokenCloseMacro(stream, state) {
      if(stream.eat('>')) {
        state.tokenize = null;
        return 'tag bracket';
      }

      stream.eatWhile(/[^\>]/);
      var macroName = '@'+stream.current();

      if(macroName != state.context.type) {
        return 'error';
      }

      popContext(state);

      return 'variable-3';
    }



    return {
      startState: function(){
        return {
          tokenize: null, 
          advancing: true,
          context: new Context(0, 0, "top", false),
          indented: 0
        };
      },

      token: function(stream, state){
        if(stream.eatSpace()) return null;

        state.indented = stream.indentation();

        if(state.tokenize) {
          var style = state.tokenize(stream, state);
          prevContent = stream.current();
          return style;
        }

        while(state.context.type.match(/[\)\]\}]/)) {
          popContext(state);
        }

        if(stream.match('${')) {
          state.tokenize = tokenBase('}','bracket');
          return 'bracket';
        }

        if(stream.match('<#')) {
          if(stream.match('--')) {
            state.tokenize = tokenComment;
            return 'comment';
          }

          state.tokenize = tokenOpenDirective;
          return 'tag bracket';
        }

        if(stream.match('<@')) {
          state.tokenize = tokenOpenMacro;
          return 'tag bracket';
        }

        if(stream.match('</#')) {
          state.tokenize = tokenCloseDirective;
          return 'tag bracket';
        }

        if(stream.match('</@')) {
          state.tokenize = tokenCloseMacro;
          return 'tag bracket';
        }

        state.advancing = false;

        return null;
      },
      indent: function(state, textAfter){

        if (state.tokenize == tokenComment) return CodeMirror.Pass;

        var ctx = state.context, firstChar = textAfter && textAfter.charAt(0);
        
        var closing = true;
        if(ctx.type == 'top') {
          closing = true;
        } else if(ctx.type.match(/[\}\]\)]/)) {
          closing = firstChar == ctx.type;
        } else if(ctx.type.indexOf('@') == 0) {
          closing = /^\s*\<\/\@/.test(textAfter);
        } else if('if else elseif'.indexOf(ctx.type) >= 0) {
          closing = /^\s*<(#else|\/#if)/.test(textAfter);
        } else if('case default'.indexOf(ctx.type) >= 0) {
          closing = /^\s*<#(case|default)/.test(textAfter);
        } else if(parserConfig.selfCloseDirective.propertyIsEnumerable(ctx.type)) {
          closing = true;
        } else {
          closing = /^\s*\<\/\#/.test(textAfter);
        }

        if (ctx.align) return ctx.column + (closing ? 0 : 1);
        else return ctx.indented + (closing ? 0 : config.indentUnit);
      },
      blockCommentStart: "<#--",
      blockCommentEnd: "-->",
      electricInput: /<\/[\#\@][\s\w\.]+>$/,
      fold: ["comment","xml","brace"]
    };
  });


  CodeMirror.defineMode("freemarker", function(config) {
    var htmlMode = CodeMirror.getMode(config, "text/html");
    var ftlMode = CodeMirror.getMode(config, {
      name: 'ftl:inner',
      directive: words(pairCloseDirective + selfCloseDirective),
      selfCloseDirective: words(selfCloseDirective),
      builtin: words(builtin),
      atoms: words(atoms)
    });

    function dispatch(stream, state) {
      var isFtl = state.curMode === ftlMode;
      if(!isFtl) {
        if(stream.match(/^(<\/?\#)|(<\/?\@)|(\$\{)/, false)) {
          state.curMode = ftlMode;
          state.curState = state.ftl; 
          state.curState.advancing = true;
          state.curState.indented = stream.indentation();
          return null;
        }

      } else if(isFtl && !state.ftl.advancing) {

        state.curMode = htmlMode;
        state.curState = state.html;
        state.curState.htmlState.indented = stream.indentation();
        return null;
      }

      return state.curMode.token(stream, state.curState);
    }

    return {
      startState: function() {
        var html = CodeMirror.startState(htmlMode), ftl = CodeMirror.startState(ftlMode);
        return {
          html: html,
          ftl: ftl,
          curMode: ftlMode,
          curState: ftl,
          pending: null
        };
      },

      copyState: function(state) {
        var html = state.html, htmlNew = CodeMirror.copyState(htmlMode, html),
            ftl = state.ftl, ftlNew = CodeMirror.copyState(ftlMode, ftl), cur;
        if (state.curMode == htmlMode) cur = htmlNew;
        else cur = ftlNew;
        return {html: htmlNew, ftl: ftlNew, curMode: state.curMode, curState: cur,
                pending: state.pending};
      },

      token: dispatch,

      indent: function(state, textAfter){
        if ((state.curMode != ftlMode && /^\s*<\/?[\#\@]/.test(textAfter))) {
          return ftlMode.indent(state.ftl, textAfter);
        } 

        var indent = state.curMode.indent(state.curState, textAfter);
          
        if(state.curMode == htmlMode && state.html.htmlState && state.html.htmlState.context && indent == 0) {
            indent = state.html.htmlState.context.indent;
        }
        return indent;
      },
      innerMode: function(state) { return {state: state.curState, mode: state.curMode};}
    };
  });

  CodeMirror.defineMIME("text/x-freemarker", "freemarker");

});
