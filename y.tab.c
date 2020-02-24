/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "mini_l.y" /* yacc.c:339  */

 #include <stdio.h>
 #include <stdlib.h>
 void yyerror(const char *msg);
 extern int currLine;
 extern int currPos;
 FILE * yyin;

#line 75 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    IDENT = 258,
    NUMBER = 259,
    FUNCTION = 260,
    BEGIN_PARAMS = 261,
    END_PARAMS = 262,
    BEGIN_LOCALS = 263,
    END_LOCALS = 264,
    BEGIN_BODY = 265,
    END_BODY = 266,
    INTEGER = 267,
    ARRAY = 268,
    OF = 269,
    IF = 270,
    THEN = 271,
    ENDIF = 272,
    ELSE = 273,
    WHILE = 274,
    DO = 275,
    FOR = 276,
    BEGINLOOP = 277,
    ENDLOOP = 278,
    CONTINUE = 279,
    READ = 280,
    WRITE = 281,
    TRUE = 282,
    FALSE = 283,
    RETURN = 284,
    SEMICOLON = 285,
    COLON = 286,
    COMMA = 287,
    L_PAREN = 288,
    R_PAREN = 289,
    L_SQUARE_BRACKET = 290,
    R_SQUARE_BRACKET = 291,
    AND = 292,
    OR = 293,
    SUB = 294,
    ADD = 295,
    MULT = 296,
    DIV = 297,
    MOD = 298,
    EQ = 299,
    NEQ = 300,
    LT = 301,
    GT = 302,
    LTE = 303,
    GTE = 304,
    ASSIGN = 305,
    NOT = 306
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 10 "mini_l.y" /* yacc.c:355  */

  char* ival;
  double dval;

#line 172 "y.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 189 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  6
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   175

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  52
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  30
/* YYNRULES -- Number of rules.  */
#define YYNRULES  69
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  152

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   306

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    35,    35,    36,    39,    42,    43,    46,    47,    50,
      51,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      65,    68,    71,    74,    77,    80,    83,    86,    89,    92,
      93,    96,    97,   100,   103,   104,   108,   109,   112,   113,
     114,   115,   118,   119,   120,   121,   122,   123,   126,   127,
     128,   131,   132,   133,   136,   137,   138,   139,   142,   143,
     144,   145,   146,   147,   148,   151,   152,   155,   156,   159
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENT", "NUMBER", "FUNCTION",
  "BEGIN_PARAMS", "END_PARAMS", "BEGIN_LOCALS", "END_LOCALS", "BEGIN_BODY",
  "END_BODY", "INTEGER", "ARRAY", "OF", "IF", "THEN", "ENDIF", "ELSE",
  "WHILE", "DO", "FOR", "BEGINLOOP", "ENDLOOP", "CONTINUE", "READ",
  "WRITE", "TRUE", "FALSE", "RETURN", "SEMICOLON", "COLON", "COMMA",
  "L_PAREN", "R_PAREN", "L_SQUARE_BRACKET", "R_SQUARE_BRACKET", "AND",
  "OR", "SUB", "ADD", "MULT", "DIV", "MOD", "EQ", "NEQ", "LT", "GT", "LTE",
  "GTE", "ASSIGN", "NOT", "$accept", "prog_start", "Function",
  "Declaration_loop", "Declaration", "Ident_loop", "Statement",
  "Statement1", "Statement2", "Statement3", "Statement4", "Statement5",
  "Statement6", "Statement7", "Statement8", "Statement9", "Statement_loop",
  "ElseStatement", "Bool-Expr", "Relation-And-Expr", "Relation-Expr",
  "Relation-Expr_loop", "Comp", "Expression", "Expression_loop",
  "Multiplicative-Expr", "Term", "Var", "Var_loop", "Ident", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306
};
# endif

#define YYPACT_NINF -69

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-69)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      22,    16,    30,    22,   -69,     2,   -69,   -69,    37,    16,
      40,    27,    28,    29,    52,    16,    -8,    16,    16,   -69,
     -69,    44,   -69,    56,    71,    72,    47,   107,    67,   111,
     111,    63,    16,   -69,    16,    16,     3,    57,   -69,   -69,
     -69,   -69,   -69,   -69,   -69,   -69,   -69,    75,    38,    58,
      79,   -69,   -69,   -69,   111,     7,    45,    45,    45,    25,
      80,   -69,    60,   -69,   122,    11,   -69,   -69,   -11,    73,
     107,    50,    69,   -69,   -69,     3,   -69,   107,   -69,     3,
       3,   -69,    74,   112,   -69,     3,   -69,     3,     3,     3,
     -69,   107,   111,   -69,   -69,   -69,   -69,   -69,   -69,     3,
       3,     3,     3,   107,    76,    99,    16,    77,   -69,   -69,
      81,   -69,   -69,    89,   -69,   -69,   -69,    91,   -69,   -69,
     -69,   -69,    92,   101,    83,   102,   110,   -69,   -69,   -69,
     107,   120,     3,   -69,   118,   123,   111,   -69,   -69,   -69,
     -69,   111,   115,   -69,    16,    98,     3,   127,   107,   125,
     140,   -69
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,     2,    69,     0,     1,     3,     0,     6,
       0,     0,     0,     9,     0,     6,     0,     0,     6,     5,
       7,     0,    10,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    27,     0,     0,     0,     0,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,     0,    65,
       0,    59,    39,    40,     0,     0,     0,     0,     0,     0,
       0,    33,    34,    37,     0,    48,    54,    58,    65,     0,
       0,     0,    67,    25,    26,     0,    28,    30,     4,     0,
       0,     8,     0,     0,    61,     0,    60,     0,     0,     0,
      36,     0,     0,    42,    43,    44,    45,    46,    47,     0,
       0,     0,    53,     0,     0,     0,     0,     0,    29,    20,
       0,    41,    62,     0,    57,    56,    55,    32,    35,    38,
      49,    50,    52,     0,     0,     0,     0,    68,    66,    63,
       0,     0,    53,    64,     0,     0,     0,    31,    21,    51,
      22,     0,     0,    23,     0,     0,     0,     0,     0,     0,
       0,    24
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -69,   161,   -69,    -3,   -69,   155,   -69,   -69,   -69,   -69,
     -69,   -69,   -69,   -69,   -69,   -69,   -68,   -69,   -29,    82,
     -69,   114,   -69,    19,    43,   -50,    14,   -14,   -32,    -1
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     3,    10,    11,    12,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,   131,    60,    61,
      62,    63,    99,    64,   123,    65,    66,    67,    73,    68
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
       5,    69,   104,    74,    20,    21,     4,    51,    13,   108,
       4,    84,    19,    48,    13,    23,    13,    13,    71,     4,
      72,    72,   102,   117,    80,    82,    49,     1,     4,    51,
       6,    49,     8,    49,    49,   124,    75,   114,   115,   116,
      85,    86,    55,     9,    56,    57,    58,    14,     4,    51,
     100,   101,    52,    53,    49,    76,    48,    15,    54,    16,
      18,    17,   137,    48,    55,    25,    56,    57,    58,    49,
      87,    88,    89,    83,   127,    26,    49,    48,    75,    24,
     149,    50,    27,    28,    55,    70,    78,    77,    79,    48,
      49,    81,    72,    80,   107,   103,    91,    92,   109,   110,
     105,   106,    49,   126,   113,    49,   125,   142,   111,   130,
       4,   112,   143,   134,     4,    51,    48,   128,   119,   120,
     121,   122,    29,   129,   132,   135,    30,    31,    32,    49,
     145,    33,    34,    35,    48,   133,    36,   138,    52,    53,
     136,   140,   141,    49,    54,   144,   112,    49,   146,   148,
      55,   122,    56,    57,    58,   150,    93,    94,    95,    96,
      97,    98,    59,   151,     7,   147,    93,    94,    95,    96,
      97,    98,    22,    90,   118,   139
};

static const yytype_uint8 yycheck[] =
{
       1,    30,    70,    35,    12,    13,     3,     4,     9,    77,
       3,     4,    15,    27,    15,    18,    17,    18,    32,     3,
      34,    35,    33,    91,    35,    54,    27,     5,     3,     4,
       0,    32,    30,    34,    35,   103,    33,    87,    88,    89,
      33,    55,    39,     6,    41,    42,    43,     7,     3,     4,
      39,    40,    27,    28,    55,    36,    70,    30,    33,    31,
       8,    32,   130,    77,    39,     9,    41,    42,    43,    70,
      56,    57,    58,    54,   106,     4,    77,    91,    33,    35,
     148,    14,    10,    36,    39,    22,    11,    30,    50,   103,
      91,    12,   106,    35,    75,    22,    16,    37,    79,    80,
      50,    32,   103,     4,    85,   106,    30,   136,    34,    18,
       3,    34,   141,    30,     3,     4,   130,    36,    99,   100,
     101,   102,    15,    34,    32,    23,    19,    20,    21,   130,
     144,    24,    25,    26,   148,    34,    29,    17,    27,    28,
      30,    23,    19,   144,    33,    30,    34,   148,    50,    22,
      39,   132,    41,    42,    43,    30,    44,    45,    46,    47,
      48,    49,    51,    23,     3,   146,    44,    45,    46,    47,
      48,    49,    17,    59,    92,   132
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     5,    53,    54,     3,    81,     0,    53,    30,     6,
      55,    56,    57,    81,     7,    30,    31,    32,     8,    55,
      12,    13,    57,    55,    35,     9,     4,    10,    36,    15,
      19,    20,    21,    24,    25,    26,    29,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    79,    81,
      14,     4,    27,    28,    33,    39,    41,    42,    43,    51,
      70,    71,    72,    73,    75,    77,    78,    79,    81,    70,
      22,    79,    79,    80,    80,    33,    75,    30,    11,    50,
      35,    12,    70,    75,     4,    33,    79,    78,    78,    78,
      73,    16,    37,    44,    45,    46,    47,    48,    49,    74,
      39,    40,    33,    22,    68,    50,    32,    75,    68,    75,
      75,    34,    34,    75,    77,    77,    77,    68,    71,    75,
      75,    75,    75,    76,    68,    30,     4,    80,    36,    34,
      18,    69,    32,    34,    30,    23,    30,    68,    17,    76,
      23,    19,    70,    70,    30,    79,    50,    75,    22,    68,
      30,    23
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    52,    53,    53,    54,    55,    55,    56,    56,    57,
      57,    58,    58,    58,    58,    58,    58,    58,    58,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      68,    69,    69,    70,    71,    71,    72,    72,    73,    73,
      73,    73,    74,    74,    74,    74,    74,    74,    75,    75,
      75,    76,    76,    76,    77,    77,    77,    77,    78,    78,
      78,    78,    78,    78,    78,    79,    79,    80,    80,    81
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,    12,     3,     0,     3,     8,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     6,     6,     7,    14,     2,     2,     1,     2,     3,
       2,     2,     0,     1,     1,     3,     2,     1,     3,     1,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     3,
       3,     3,     1,     0,     1,     3,     3,     3,     1,     1,
       2,     2,     3,     4,     4,     1,     4,     1,     3,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 35 "mini_l.y" /* yacc.c:1646  */
    {printf("prog_start -> EPSILON\n");}
#line 1377 "y.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 36 "mini_l.y" /* yacc.c:1646  */
    {printf("prog_start -> Function prog_start\n");}
#line 1383 "y.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 39 "mini_l.y" /* yacc.c:1646  */
    {printf("Function -> FUNCTION Ident SEMICOLON BEGIN_PARAMS Declaration_loop END_PARAMS BEGIN_LOCALS Declaration_loop END_LOCALS BEGIN_BODY Statement_loop END_BODY\n");}
#line 1389 "y.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 42 "mini_l.y" /* yacc.c:1646  */
    {printf("Declaration_loop -> Declaration SEMICOLON Declaration_loop\n");}
#line 1395 "y.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 43 "mini_l.y" /* yacc.c:1646  */
    {printf("Declaration_loop -> EPSILON\n");}
#line 1401 "y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 46 "mini_l.y" /* yacc.c:1646  */
    {printf("Declaration -> Ident_loop COLON INTEGER\n");}
#line 1407 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 47 "mini_l.y" /* yacc.c:1646  */
    {printf("Declaration -> Ident_loop COLON ARRAY L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET OF INTEGER\n", (yyvsp[-3].dval));}
#line 1413 "y.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 50 "mini_l.y" /* yacc.c:1646  */
    {printf("Ident_loop -> Ident \n");}
#line 1419 "y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 51 "mini_l.y" /* yacc.c:1646  */
    {printf("Ident_loop -> Ident COMMA Ident_loop\n");}
#line 1425 "y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 54 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> Statement1\n");}
#line 1431 "y.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 55 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> Statement2\n");}
#line 1437 "y.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 56 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> Statement3\n");}
#line 1443 "y.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 57 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> Statement4\n");}
#line 1449 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 58 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> Statement5\n");}
#line 1455 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 59 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> Statement6\n");}
#line 1461 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 60 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> Statement7\n");}
#line 1467 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 61 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> Statement8\n");}
#line 1473 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 62 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> Statement9\n");}
#line 1479 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 65 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement1 -> Var ASSIGN Expression\n");}
#line 1485 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 68 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> IF Bool-Expr THEN Statement_loop ElseStatement ENDIF\n");}
#line 1491 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 71 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> WHILE Bool-Expr BEGINLOOP Statement_loop SEMICOLON ENDLOOP\n");}
#line 1497 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 74 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> DO BEGINLOOP Statement_loop SEMICOLON ENDLOOP WHILE Bool-Expr\n");}
#line 1503 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 77 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> FOR Var ASSIGN NUMBER SEMICOLON Bool-Expr SEMICOLON Var ASSIGN Expression BEGINLOOP Statement_loop SEMICOLON ENDLOOP\n");}
#line 1509 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 80 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> READ Var_loop\n");}
#line 1515 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 83 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> WRITE Var_loop\n");}
#line 1521 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 86 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> CONTINUE\n");}
#line 1527 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 89 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement -> Expression\n");}
#line 1533 "y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 92 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement_loop -> Statement SEMICOLON Statement_loop\n");}
#line 1539 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 93 "mini_l.y" /* yacc.c:1646  */
    {printf("Statement_loop -> Statement SEMICOLON\n");}
#line 1545 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 96 "mini_l.y" /* yacc.c:1646  */
    {printf("ElseStatement -> ELSE Statement_loop\n");}
#line 1551 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 97 "mini_l.y" /* yacc.c:1646  */
    {printf("ElseStatement -> EPSILON\n");}
#line 1557 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 100 "mini_l.y" /* yacc.c:1646  */
    {printf("Bool-Expr -> Relation-And-Expr\n");}
#line 1563 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 103 "mini_l.y" /* yacc.c:1646  */
    {printf("Relation-And-Expr -> Relation-Expr\n");}
#line 1569 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 104 "mini_l.y" /* yacc.c:1646  */
    {printf("Relation-And-Expr -> Relation-Expr AND Relation-And-Expr\n");}
#line 1575 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 108 "mini_l.y" /* yacc.c:1646  */
    {printf("Relation-Expr -> NOT Relation-Expr_loop\n");}
#line 1581 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 109 "mini_l.y" /* yacc.c:1646  */
    {printf("Relation-Expr -> Relation-Expr_loop\n");}
#line 1587 "y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 112 "mini_l.y" /* yacc.c:1646  */
    {printf("Relation-Expr_loop -> Expression Comp Expression\n");}
#line 1593 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 113 "mini_l.y" /* yacc.c:1646  */
    {printf("Relation-Expr_loop -> TRUE\n");}
#line 1599 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 114 "mini_l.y" /* yacc.c:1646  */
    {printf("Relation-Expr_loop -> FALSE\n");}
#line 1605 "y.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 115 "mini_l.y" /* yacc.c:1646  */
    {printf("Relation-Expr_loop -> L_PAREN Bool-Expr R_PAREN\n");}
#line 1611 "y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 118 "mini_l.y" /* yacc.c:1646  */
    {printf("Comp -> EQ\n");}
#line 1617 "y.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 119 "mini_l.y" /* yacc.c:1646  */
    {printf("Comp -> NEQ\n");}
#line 1623 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 120 "mini_l.y" /* yacc.c:1646  */
    {printf("Comp -> LT\n");}
#line 1629 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 121 "mini_l.y" /* yacc.c:1646  */
    {printf("Comp -> GT\n");}
#line 1635 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 122 "mini_l.y" /* yacc.c:1646  */
    {printf("Comp -> LTE\n");}
#line 1641 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 123 "mini_l.y" /* yacc.c:1646  */
    {printf("Comp -> GTE\n");}
#line 1647 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 126 "mini_l.y" /* yacc.c:1646  */
    {printf("Expression -> Multiplicative-Expr\n");}
#line 1653 "y.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 127 "mini_l.y" /* yacc.c:1646  */
    {printf("Expression -> Multiplicative-Expr SUB Expression\n");}
#line 1659 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 128 "mini_l.y" /* yacc.c:1646  */
    {printf("Expression -> Multiplicative-Expr ADD Expression\n");}
#line 1665 "y.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 131 "mini_l.y" /* yacc.c:1646  */
    {printf("Expression -> COMMA Expression\n");}
#line 1671 "y.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 132 "mini_l.y" /* yacc.c:1646  */
    {printf("Expression_loop -> Expression\n");}
#line 1677 "y.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 133 "mini_l.y" /* yacc.c:1646  */
    {printf("Expression_loop -> EPSILON\n");}
#line 1683 "y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 136 "mini_l.y" /* yacc.c:1646  */
    {printf("Multiplicative-Expr -> Term\n");}
#line 1689 "y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 137 "mini_l.y" /* yacc.c:1646  */
    {printf("Multiplicative-Expr -> Term MOD Term\n");}
#line 1695 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 138 "mini_l.y" /* yacc.c:1646  */
    {printf("Multiplicative-Expr -> Term MOD Term\n");}
#line 1701 "y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 139 "mini_l.y" /* yacc.c:1646  */
    {printf("Multiplicative-Expr-> Term MOD Term\n");}
#line 1707 "y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 142 "mini_l.y" /* yacc.c:1646  */
    {printf("Term -> Var\n");}
#line 1713 "y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 143 "mini_l.y" /* yacc.c:1646  */
    {printf("Term -> NUMBER %d\n", (yyvsp[0].dval));}
#line 1719 "y.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 144 "mini_l.y" /* yacc.c:1646  */
    {printf("Term -> SUB Var\n");}
#line 1725 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 145 "mini_l.y" /* yacc.c:1646  */
    {printf("Term -> SUB NUMBER %d\n", (yyvsp[0].dval));}
#line 1731 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 146 "mini_l.y" /* yacc.c:1646  */
    {printf("Term -> L_PAREN Expression R_PAREN\n");}
#line 1737 "y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 147 "mini_l.y" /* yacc.c:1646  */
    {printf("Term -> SUB L_PAREN Expression R_PAREN\n");}
#line 1743 "y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 148 "mini_l.y" /* yacc.c:1646  */
    {printf("Term -> Ident L_PAREN Expression R_PAREN\n");}
#line 1749 "y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 151 "mini_l.y" /* yacc.c:1646  */
    {printf("Var -> Ident\n");}
#line 1755 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 152 "mini_l.y" /* yacc.c:1646  */
    {printf("Var -> Ident L_SQUARE_BRACKET Expression R_SQUARE_BRACKET\n");}
#line 1761 "y.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 155 "mini_l.y" /* yacc.c:1646  */
    {printf("Var_loop -> Var\n");}
#line 1767 "y.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 156 "mini_l.y" /* yacc.c:1646  */
    {printf("Var_loop -> Var COMMA Var_loop\n");}
#line 1773 "y.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 160 "mini_l.y" /* yacc.c:1646  */
    {printf("Ident -> IDENT %s \n", (yyvsp[0].ival));}
#line 1779 "y.tab.c" /* yacc.c:1646  */
    break;


#line 1783 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 161 "mini_l.y" /* yacc.c:1906  */


void yyerror(const char* msg)
{
  extern int currLine;
  extern char* yytext;

  printf("ERROR: %s at symbol \"%s\" on line %d\n", msg, yytext, currLine);
  exit(1);
}
