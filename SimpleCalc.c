#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

int no_of_errors;
int no_of_strings=0;

#define MAXLINE 1000 /* max length for input data */
char g_inData[MAXLINE]; /* storage for input data*/

typedef enum  { FALSE, TRUE } boolean;
typedef enum _TokenCode
{
	START,
	UNKNOWN,
	OPERATION,
	NUMBER,
	EXIT,
	END
} TokenCode;

typedef enum _OperationCode
{
	OPERATOR,
	FUNC
} OperationCode,*POperationCode;

typedef double (*POPERACTION)(double arg1);

typedef struct Operation
{
	OperationCode code;
	char mnemonic[4];
	int  lenMnem;
	int priority;
	int specs;
	POPERACTION Action;
} Operation, *POperation;

typedef enum _SpecsCode
{
	NONE = 0,
	NOT_BEFORE_OP = 1
} SpecsCode;


typedef struct tagToken {
	TokenCode code;
	char*	  valueStr;
	double	  value;
	POperation pOperation;
	int		  priority;

	int (*isToken)(char** inData,struct tagToken* token);

} TOKEN, *PTOKEN;

typedef int (*PISTOKEN)(char** inData,PTOKEN token);

TOKEN g_curToken = {START,0x0};


#define _MAX_OPERATIONS 50
Operation g_Operations1[_MAX_OPERATIONS];
Operation g_Operations2[_MAX_OPERATIONS];
Operation g_Operations3[_MAX_OPERATIONS];

#define _MAX_PRIORITY 3

POperation g_Operations[_MAX_PRIORITY] = {g_Operations1,g_Operations2,g_Operations3};
int g_countOperations[_MAX_PRIORITY] = {0,0,0};


typedef struct tagMnemonic
{
	char* mnemonic;
	POperation pOperation;
} MNEMONIC, *PMNEMONIC;

MNEMONIC g_OperationsMnems[_MAX_OPERATIONS*_MAX_PRIORITY];
int g_countMnems = 0;

void AddOperation(const char* mnem,POperationCode code,int priority,int specs,POPERACTION Action)
{
	POperation operTableForPriority;
	int *countOpersForPriority;
	POperation newOperation;
	if(priority > _MAX_PRIORITY || priority < 1)
		return;
	operTableForPriority = g_Operations[priority-1];
	countOpersForPriority = &g_countOperations[priority-1];
	newOperation = &operTableForPriority[*countOpersForPriority];
	strcpy(newOperation->mnemonic,mnem);
	newOperation->priority=priority;
	newOperation->code = *code;
	newOperation->Action = Action;
	newOperation->specs = specs;
	(*countOpersForPriority)++;

	g_OperationsMnems[g_countMnems].mnemonic = (char*)&newOperation->mnemonic;
	g_OperationsMnems[g_countMnems].pOperation = newOperation;
	g_countMnems++;
}

int countAllMnems()
{
	return g_countMnems;
}

double PLUS(double arg1);
double MINUS(double arg1);
double MUL(double arg1);
double DIV(double arg1);
double LP(double arg1);
double RP(double arg1);

double UMINUS(double arg1);

#define NO_SPECS 0 

void CalcInit()
{

	OperationCode opCode = OPERATOR;
	AddOperation("+",&opCode,1,NO_SPECS,PLUS);
	AddOperation("-",&opCode,1,NOT_BEFORE_OP,MINUS);
	AddOperation("/",&opCode,2,NO_SPECS,DIV);
	AddOperation("*",&opCode,2,NO_SPECS,MUL);
	AddOperation("(",&opCode,3,NO_SPECS,LP);
	AddOperation(")",&opCode,3,NO_SPECS,RP);
	AddOperation("-",&opCode,3,NO_SPECS,UMINUS);
}

int isNumberToken(char** inData,PTOKEN token);
int isOperationToken(char** inData,PTOKEN token);
int isExitToken(char** inData,PTOKEN token);
int isEndToken(char** inData,PTOKEN token);

TOKEN g_TokensTable[] = { {NUMBER,0x0,0.0,0x0,0,isNumberToken},
							{OPERATION,0x0,0.0,0x0,0,isOperationToken},
							{EXIT,0x0,0.0,0x0,0,isExitToken},
							{END,0x0,0.0,0x0,0,isEndToken}
};


double expr(boolean get,int priority);
int error(const char* s);
TokenCode get_token();

double expr(boolean get,int priority)
{
	double left = 1;
	if(priority<1 || priority > _MAX_PRIORITY)
	{
		g_curToken.code = UNKNOWN;
		return error("PRIORITY INVALID");
	}
	if(priority == _MAX_PRIORITY)
		{
			if(get)
				if(get_token() == UNKNOWN)
					return error("INCORRECT EXPRESSION");
		}
	else
		left=expr(get,priority+1);
	for(;;)
	{
		switch(g_curToken.code)
		{
			case NUMBER	:
			{
				double v=g_curToken.value;
				if(get_token() == UNKNOWN)
				{
					return error("INCORRECT EXPRESSION");
				}
				return v;
			};
			case UNKNOWN	:
			case EXIT	:
				return 1;
			case OPERATION	:
				{
					if(g_curToken.priority == priority)
					{

						int countOpers = 0;
						POperation pOperation = g_curToken.pOperation;
						
						left = pOperation->Action(left);

					}
					else 
						return left;
					if(priority == _MAX_PRIORITY)
						return left;
					break;
				}
			default		:
				{
					if(priority == _MAX_PRIORITY)
					{
						g_curToken.code = UNKNOWN;
						return error("INCORRECT EXPRESSION");
					}
					return left;
				}
		};
	};
};


double PLUS(double arg1)
{
	arg1+=expr(TRUE,2);
	return arg1;
}
double MINUS(double arg1)
{
	arg1-=expr(TRUE,2);
	return arg1;
}
double DIV(double arg1)
{
	double d=expr(TRUE,3);
	if(d != 0.0)
	{
		arg1/=d;
		return arg1;
	};
	g_curToken.code = UNKNOWN;
	return error("DIVIZION BY ZERO");
}
double MUL(double arg1)
{
	arg1*=expr(TRUE,2);
	return arg1;
}
double LP(double arg1)
{
	double e=expr(TRUE,1);
	if(g_curToken.code == UNKNOWN)
		return 1;

	if(g_curToken.code == OPERATION && (g_curToken.pOperation != 0x0 && g_curToken.pOperation->mnemonic[0] == ')'))
	{
		get_token();
		return e;
	}
	g_curToken.code = UNKNOWN;
	return error(") EXPECTED");
}
double RP(double arg1)
{
	return 1;
}

double UMINUS(double arg1)
{
	return -expr(TRUE,3);
}


int countAllTokens();

char* g_curPosInStreamData = 0x0; // pointer on current data to parse
TokenCode get_token()
{
	int countTokens = countAllTokens();
	char ch=0;
	char** data = &g_curPosInStreamData;
	int i=0;
	ch = **data;
	while(*data[i]!='\n'&&isspace(*data[i]))
	{
		if(!(*data[i++])) return g_curToken.code=END;
	}
	*data += i;
	for(i=0;i<countTokens;i++)
	{
		TOKEN curToken = g_TokensTable[i];
		if(curToken.isToken != 0x0)
		{
			int result = curToken.isToken(data,&curToken);
			if(result != 1)
				continue;
			g_curToken = curToken;
			return g_curToken.code;
		}

	}
	g_curToken.code = UNKNOWN;
	return UNKNOWN;
};

int isNumberToken(char** data,PTOKEN token)
{
	char ch = **data;
	int i=0;

	char* temp = *data;
	if(isdigit(*temp))
	{
		char buf[100];
		token->code = NUMBER;
		token->valueStr = temp;
		i=0;
		while(isdigit(*temp)||(*temp=='.'))
		{	
			buf[i++] = *temp++;
		}
		*data = temp;
		token->value = atof(buf);
		return 1;

	}

	return 0;
}
int isOperationToken(char** data,PTOKEN token)
{
	char ch = **data;
	int i=0;
	POperation pOperation;
	for(i=0;i<countAllMnems();i++)
	{
		char* pCurMnem = g_OperationsMnems[i].mnemonic;

		if(strncmp(pCurMnem,*data,strlen(pCurMnem))!=0)
			continue;

		pOperation = g_OperationsMnems[i].pOperation;

		if(pOperation->specs != NONE)
		{
			if( (pOperation->specs & (int)NOT_BEFORE_OP) )
			{
				if(g_curToken.code == OPERATION)
					continue;
			}
		}

		token->code = OPERATION;
		token->valueStr = *data;
		token->priority = pOperation->priority;
		token->pOperation = pOperation;
		*data += strlen(pCurMnem);
		return 1;
	}

	return 0;
}
int isEndToken(char** data,PTOKEN token)
{
	char ch = **data;
	switch(ch)
	{
		case 0	:
		case '\n'	:
			{
				token->code = END;
				token->valueStr = *data;
				return 1;
			}
	};

	return 0;
}
int isExitToken(char** data,PTOKEN token)
{
	char ch = **data;
	switch(ch)
	{
		case ';'	:
			{
				token->code = EXIT;
				token->valueStr = *data;
				return 1;
			}
	};

	return 0;
}

int error(const char* s)
{
	no_of_errors++;
	fprintf(stderr, "ERROR: %s\n\n",s);
	return 1;
};

int countAllTokens()
{
	return sizeof(g_TokensTable)/sizeof(TOKEN);
}


int getlineEx(char* desStr,int maxLength);
int main(int argc, char* argv[])
{
	CalcInit();
	while(g_curToken.code != EXIT)
	{
		printf("Enter the expression or type ';' to exit:\n");
		getlineEx(g_inData,MAXLINE);
		g_curPosInStreamData = g_inData;
		g_curToken.code = START;
		while(g_curToken.code != END && g_curToken.code != EXIT)
		{
			double result;
			long res2;
			get_token();
			result = expr(FALSE,1);
			if(g_curToken.code == UNKNOWN || g_curToken.code == EXIT)
				break;
			res2 = (long)result;

			if((double)res2 == result)
				printf("Result: %d\n\n",(long)result);
			else
				printf("Result: %f\n\n",result);
		};
	}
	return no_of_errors;
}; 
/* function getline: 
*	copy stdin data to destStr
*/
int getlineEx(char* destStr,int maxLength)
{
	int c, i;
	for (i=0; i < maxLength-1 && (c=getchar()) != EOF && c != '\n'; ++i)
	destStr[i] = c;
	if(c == '\n') {
		destStr[i]= c;
		++i;
	}
	destStr[i] = '\0';
	return i;
}
