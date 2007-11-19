/* erlyjs.c */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "erl_interface.h"
#include "ei.h"
#include <jsapi.h>
#include <jsparse.h>
#include <jsscan.h>
#include <jsatom.h>
#include <jsfun.h>
#include <jsscope.h>
#include <jsinterp.h>

#define BUFSIZE 1000
#define KEY_LENGTH 100
#define JSERL_OK 0
#define JSERL_ERR -1

/* from jsscan.h */
const char * TOKENS[81] = {
	"EOF", "EOL", "SEMI", "COMMA", "ASSIGN", "HOOK", "COLON", "OR", "AND",
	"BITOR", "BITXOR", "BITAND", "EQOP", "RELOP", "SHOP", "PLUS", "MINUS", "STAR",
  	"DIVOP", "UNARYOP", "INC", "DEC", "DOT", "LB", "RB", "LC", "RC", "LP", "RP",
  	"NAME", "NUMBER", "STRING", "OBJECT", "PRIMARY", "FUNCTION", "EXPORT",
  	"IMPORT", "IF", "ELSE", "SWITCH", "CASE", "DEFAULT", "WHILE", "DO", "FOR",
  	"BREAK", "CONTINUE", "IN", "VAR", "WITH", "RETURN", "NEW", "DELETE",
  	"DEFSHARP", "USESHARP", "TRY", "CATCH", "FINALLY", "THROW", "INSTANCEOF",
  	"DEBUGGER", "XMLSTAGO", "XMLETAGO", "XMLPTAGC", "XMLTAGC", "XMLNAME",
  	"XMLATTR", "XMLSPACE", "XMLTEXT", "XMLCOMMENT", "XMLCDATA", "XMLPI", "AT",
  	"DBLCOLON", "ANYNAME", "DBLDOT", "FILTER", "XMLELEM", "XMLLIST", "RESERVED",
  	"LIMIT",
};

const int NUM_TOKENS = sizeof(TOKENS) / sizeof(TOKENS[0]);

typedef unsigned char byte;

ETERM * traverse(JSParseNode * root, JSContext * context);

int main(int argc, char *argv[]) {
 	int fd;                           /* fd to Erlang node */
  	unsigned char buf[BUFSIZE];       /* Buffer for incoming message */
  	ErlMessage emsg;                  /* Incoming message */
  	int number;                       /* C-Node number */
  	char *cookie;                     /* Shared cookie */
  	short creation;                   /* ?? */
  	char *erlang_node;                /* Erlang node to connect to */
  	ETERM *from, *args, *arg1, *ast;
  	int received, status, loop = 1;  
	number = atoi(argv[1]);
	cookie = argv[2];
	creation = 0;
	erlang_node = argv[3];
	char *script;

  	JSRuntime * runtime;
  	JSContext * context;
  	JSObject * global;
  	JSTokenStream * token_stream;
  	JSParseNode * node;
	jschar *source;

  	erl_init(NULL, 0);

  	if (!erl_connect_init(number, cookie, creation))
    	erl_err_quit("erl_connect_init");

  	if ((fd = erl_connect(erlang_node)) < 0)
    	erl_err_quit("erl_connect");

  	fprintf(stderr, "jserl (c-node %d) started. \n\r", number);  
    
  	while (loop) {
    	received = erl_receive_msg(fd, buf, BUFSIZE, &emsg);

    	if (received == ERL_TICK) {
      		/* ignore */    
    	} else if (received == ERL_ERROR) {
      		loop = 0;
    	} else 	if (emsg.type == ERL_REG_SEND) {
        	from = erl_element(2, emsg.msg);
        	args = erl_element(3, emsg.msg);  
        	arg1 = erl_element(1, args);
			script = (char *)ERL_ATOM_PTR(arg1);

  			runtime = JS_NewRuntime(8L * 1024L * 1024L);
  			if (runtime) {
				context = JS_NewContext(runtime, 8192);
	  			if (context) {
					global = JS_NewObject(context, NULL, NULL, NULL);
					if (global) {						
						if (JS_InitStandardClasses(context, global)) {
							source = JS_GetStringChars(JS_NewString(context, script, strlen(script)));
							token_stream = js_NewBufferTokenStream(context, source, strlen(script));
							if (token_stream) {								
								node = js_ParseTokenStream(context, global, token_stream);
				  				if (node) {
									ast = traverse(node, context); 
									if (ast) {
										erl_send(fd, from, ast);
									} else {
										erl_send(fd, from, erl_format("{error, ~s}", "AST traversal error"));
									}
							 	} else {
									erl_send(fd, from, erl_format("{error, ~s}", "parse error in file"));	
								}																															
							} else {
								erl_send(fd, from, erl_format("{error, ~s}", "cannot create token stream from"));
							}  								
						} else {
							erl_send(fd, from, erl_format("{error, ~s}", "cannot initialize standard classes"));
						}									
					} else {
						erl_send(fd, from, erl_format("{error, ~s}", "cannot create JS global object"));
					}								
				} else {
					erl_send(fd, from, erl_format("{error, ~s}", "cannot create JS context"));
				}	
  			} else {
				erl_send(fd, from, erl_format("{error, ~s}", "cannot create JS runtime"));
			}
    	}
		erl_free_term(from); 
		erl_free_compound(ast);	   	
		JS_DestroyContext(context);
		JS_DestroyRuntime(runtime);	
		erl_free_term(emsg.from); 
		erl_free_term(emsg.msg);
  	}
  	fprintf(stderr, "jserl loop exit\n\r"); 
  	exit(EXIT_SUCCESS);
}


ETERM * traverse(JSParseNode * root, JSContext * context) {
	ETERM * type=NULL, * ast=NULL;
	ETERM * ast1=NULL, * ast2=NULL, * ast3=NULL;
	ETERM * list=NULL, * head=NULL;
	
	JSParseNode * node;
	JSAtom * atom;
	JSObject * object;
	JSFunction * function;
	JSString * string;
	JSAtom ** params;
	JSScope * scope;
	JSScopeProperty * scope_property;
	
	char *cstr;
	int i;
					
  	if (root == NULL) {
		return NULL;
	} else if (root->pn_type >= NUM_TOKENS) {
    	return NULL;
  	} else {
		type = erl_format("{~a, {~i, ~i, ~i, ~i}}", 
			TOKENS[root->pn_type],
       		root->pn_pos.begin.lineno, 
			root->pn_pos.begin.index,
       		root->pn_pos.end.lineno, 
			root->pn_pos.end.index);
		switch (root->pn_arity) {
			case PN_UNARY:
				ast1 = traverse(root->pn_kid, context);
				ast = erl_format("{~w, ~w}", type, ast1);
    			break;
			case PN_BINARY:
				ast1 = traverse(root->pn_left, context);
				ast2 = traverse(root->pn_right, context);			
				ast = erl_format("{~w, ~w, ~w}",	type, ast1, ast2);
	    		break;
	  		case PN_TERNARY:
				ast1 = traverse(root->pn_kid1, context),
				ast2 = traverse(root->pn_kid2, context),
				ast3 = traverse(root->pn_kid3, context),
				ast = erl_format("{~w, ~w, ~w, ~w}", type, ast1, ast2, ast3);
	    		break;
			case PN_LIST:
				list = erl_mk_empty_list();
				for (node = root->pn_head; node != NULL; node = node->pn_next) {  
					head = traverse(node, context);
					list = erl_cons(head, list); 
				}	
				ast = erl_format("{~w, ~w}", type, list);
	    		break;	
	 		case PN_FUNC:
				object = ATOM_TO_OBJECT(root->pn_funAtom);
				function = (JSFunction *) JS_GetPrivate(context, object);
				
				/* function name */
		    	if (function->atom) {
		      		string = ATOM_TO_STRING(function->atom);
					cstr = JS_GetStringBytes(string);
		    	} else {
					cstr = "anonymTODO";
				}
				
			    /* function parameters */
			    params = malloc(function->nargs * sizeof(JSAtom *));
			    for (i = 0; i < function->nargs; i++) {
			      /* initialize to NULL for sanity check */
			      params[i] = NULL;
			    }		
			
				scope = OBJ_SCOPE(object);
				for (scope_property = SCOPE_LAST_PROP(scope);
				    scope_property != NULL;
				    scope_property = scope_property->parent) {
				        if (scope_property->getter != js_GetArgument) {
				            continue;
				        }
				        params[(uint16) scope_property->shortid] = JSID_TO_ATOM(scope_property->id);
				}
				
				list = erl_mk_empty_list();
				for (i = 0; i < function->nargs; i++) {
				    /* params[i] is a JSAtom * containing the parameter */
					list = erl_cons(erl_mk_atom(JS_GetStringBytes(ATOM_TO_STRING(params[i]))), list); 
				}
				free(params);	
				ast1 = traverse(root->pn_body, context);					
				ast = erl_format("{~w, ~a, ~w, ~w}", type, cstr, list, ast1);
				
				break;
	  		case PN_NAME:			
				string = ATOM_TO_STRING(root->pn_atom);
				cstr = JS_GetStringBytes(string);
				ast = erl_format("{~w, ~a}", type, cstr);
				break;
	  		case PN_NULLARY:
				if (TOKENS[root->pn_type] == "NUMBER") {
					ast = erl_format("{~w, ~f}", type, root->pn_dval);
				} else if (TOKENS[root->pn_type] == "STRING") {					
					string = ATOM_TO_STRING(root->pn_atom);
					cstr = JS_GetStringBytes(string);
					ast = erl_format("{~w, ~s}", type, cstr);
				} else {
					ast = erl_format("{~w, ~s}", type, "other");					
				}			
				break;
  			default:
				ast = erl_format("{~w, ~s}", type, "unknown");
		}
		return ast;
	}
}
