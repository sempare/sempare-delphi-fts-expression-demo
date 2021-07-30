(*%*************************************************************************************************
 *                 ___                                                                              *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                                      *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                                     *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                                     *
 *                                    |_|                                                           *
 ****************************************************************************************************
 *         https://github.com/sempare/sempare-delphi-fts-expression                                 *
 ****************************************************************************************************
 *                                                                                                  *
 * Copyright (c) 2021 Sempare Limited                                                               *
 *                                                                                                  *
 * Contact: info@sempare.ltd                                                                        *
 *                                                                                                  *
 * License: GPL V3.0                                                                                *
 *                                                                                                  *
 *************************************************************************************************%*)
unit Sempare.FullTextSearch.Expression;

interface

uses
  System.SysUtils,
  System.Classes;

type
  TTerminal = ( //
    etString, etDate, etTime, etNumber, etID, //
    etOpenBracket, etCloseBracket, etOpenSquareBracket, etCloseSquareBracket, //
    etEq, etNotEQ, etLT, etLTE, etGT, etGTE, etComma, //
    etContains, etStarts, etEnds, etRegex, etLike, etBetween, etWITH, //
    etNOT, etAND, etOR, //
    etEOF //
    );

const
  TNegTerminal: array [TTerminal] of TTerminal = ( //
    etString, etDate, etTime, etNumber, etID, //
    etOpenBracket, etCloseBracket, etOpenSquareBracket, etCloseSquareBracket, //
    etNotEQ, etEq, etGT, etGTE, etLT, etLTE, etComma, //
    etContains, etStarts, etEnds, etRegex, etLike, etBetween, etWITH, //
    etNOT, etAND, etOR, //
    etEOF //
    );

type
  TSymbol = class
  private
    FTerminal: TTerminal;
    FValue: variant;
  public
    constructor Create(const ATerminal: TTerminal; const AValue: variant);

    property Terminal: TTerminal read FTerminal;
    property Value: variant read FValue;
  end;

  TExpr = class abstract
  protected
    function GetExprString: string; virtual; abstract;
  public
    property ExprString: string read GetExprString;
  end;

  TValueExpr<T> = class abstract(TExpr)
  private
    FValue: T;
  public
    constructor Create(const AValue: T);
    property Value: T read FValue;
  end;

  TIdExpr = class(TValueExpr<string>)
  protected
    function GetExprString: string; override;
  end;

  TStringExpr = class(TValueExpr<string>)
  protected
    function GetExprString: string; override;
  end;

  TNumberExpr = class(TValueExpr<double>)
  protected
    function GetExprString: string; override;
  end;

  TDateExpr = class(TValueExpr<TDate>)
  protected
    function GetExprString: string; override;
  end;

  TTimeExpr = class(TValueExpr<TTime>)
  protected
    function GetExprString: string; override;
  end;

  TTerminalExpr = class abstract(TExpr)
  private
    FTerminal: TTerminal;
    FNot: boolean;
  public
    constructor Create(const ATerminal: TTerminal; const ANot: boolean);
    property Terminal: TTerminal read FTerminal;
  end;

  TUnaryExpr = class(TTerminalExpr)
  private
    FExpr: TExpr;
  protected
    function GetExprString: string; override;
  public
    constructor Create(const ATerminal: TTerminal; const AExpr: TExpr);
    destructor Destroy; override;
  end;

  TBinaryExpr = class(TTerminalExpr)
  private
    FLeftExpr: TExpr;
    FRightExpr: TExpr;
  protected
    function GetExprString: string; override;
  public
    constructor Create(const ATerminal: TTerminal; const ALeftExpr, ARightExpr: TExpr);
    destructor Destroy; override;
  end;

  TSimpleExpr = class(TTerminalExpr)
  private
    FId: string;
    FValue: variant;
  protected
    function GetExprString: string; override;
  public
    constructor Create(const ATerminal: TTerminal; const AId: string; const AValue: variant; const ANot: boolean);
  end;

  TBetweenExpr = class(TTerminalExpr)
  private
    FId: string;
    FStart: variant;
    FEnd: variant;
  protected
    function GetExprString: string; override;
  public
    constructor Create(const AId: string; const AStart: variant; const AEnd: variant; const ANot: boolean);
  end;

  TLexer = class
  private
    FStream: TStream;
    FReader: TStreamReader;
    FNextChar: char;
    FHasNext: boolean;
    FValue: string;
    FEnd: boolean;
    procedure LexerError;
    function CreateSymbol(const ATerminal: TTerminal): TSymbol;
    procedure PushChar(const AChar: char);
    function GetChar: char;

  public
    constructor Create(const AStream: TStream);
    destructor Destroy; override;
    function HasMore: boolean;
    function GetNext: TSymbol;

  end;

  TParser = class
  private
    FLexer: TLexer;
    FLookahead: TSymbol;
    function Match(const ATerminal: TTerminal): string;
    function MatchExpr(const ADoOr: boolean = true): TExpr;
    function MatchValueList: TArray<variant>;
    function MatchValueLike: variant;
    function MatchValue: variant;
    function MatchOperator: TTerminal;
    procedure ParseError;

  public
    constructor Create(const aLexer: TLexer);
    destructor Destroy; override;

    function Parse(): TExpr;

    class function ParseStr(const AStr: string): string; static;

  end;

function GetTerminalStr(const ATerminal: TTerminal): string;

implementation

uses
  System.Variants;

function GetTerminalStr(const ATerminal: TTerminal): string;
begin
  case ATerminal of
    etAND:
      exit('and');
    etOR:
      exit('or');
    etBetween:
      exit('between');
    etContains, etStarts, etEnds, etLike, etRegex:
      exit('like');
    etLT:
      exit('<');
    etLTE:
      exit('<=');
    etGT:
      exit('>');
    etGTE:
      exit('>=');
    etEq:
      exit('=');
    etNotEQ:
      exit('!=');
  else
    raise Exception.Create('operator not supported');
  end;
end;

{ TParser }

constructor TParser.Create(const aLexer: TLexer);
begin
  FLexer := aLexer;
  FLookahead := FLexer.GetNext;
  if FLookahead = nil then
    ParseError;
end;

destructor TParser.Destroy;
begin
  FLookahead.Free;
  FLexer.Free;
  inherited;
end;

function TParser.Match(const ATerminal: TTerminal): string;
begin
  if not assigned(FLookahead) or (FLookahead.FTerminal <> ATerminal) then
  begin
    ParseError;
  end;
  result := FLookahead.FValue;
  FLookahead.Free;
  FLookahead := FLexer.GetNext;
end;

function TParser.MatchExpr(const ADoOr: boolean): TExpr;
var
  LNot: boolean;
  LID: string;
  lvalue: variant;
  lstart: variant;
  lend: variant;
  lexpr: TExpr;
  Lop: TTerminal;
begin
  result := nil;
  LNot := false;
  case FLookahead.FTerminal of
    etID:
      begin
        LID := Match(etID);

        if FLookahead.FTerminal = etNOT then
        begin
          Match(FLookahead.FTerminal);
          LNot := true;
        end;

        case FLookahead.FTerminal of
          etBetween:
            begin
              Match(etBetween);
              lstart := MatchValue;
              Match(etAND);
              lend := MatchValue;
              result := TBetweenExpr.Create(LID, lstart, lend, LNot);
            end;
          etLike:
            begin
              Match(etLike);
              lvalue := MatchValue;
              result := TSimpleExpr.Create(etLike, LID, lvalue, LNot);
            end;
        else
          begin
            Lop := MatchOperator;
            lvalue := MatchValue;
            result := TSimpleExpr.Create(Lop, LID, lvalue, LNot);
          end;
        end;
      end;
    etNOT:
      begin
        Match(etNOT);
        lexpr := MatchExpr;
        result := TUnaryExpr.Create(etNOT, lexpr);
      end;
    etOpenBracket:
      begin
        Match(etOpenBracket);
        result := MatchExpr;
        Match(etCloseBracket);
      end;
  else
    ParseError;
  end;

  if FLookahead.FTerminal = etAND then
  begin
    Match(etAND);
    result := TBinaryExpr.Create(etAND, result, MatchExpr(false));
  end;
  // we need this as seperate condition in order to ensure presidence between AND / OR
  if FLookahead.FTerminal = etOR then
  begin
    if ADoOr then
    begin
      Match(etOR);
      result := TBinaryExpr.Create(etOR, result, MatchExpr);
    end;
    exit;
  end;
  if FLookahead.FTerminal in [etEOF, etCloseBracket, etCloseSquareBracket] then
  begin
    exit;
  end;
  ParseError;
end;

function TParser.MatchOperator: TTerminal;
begin
  case FLookahead.FTerminal of
    etEq, etNotEQ, etLT, etGT, etLTE, etGTE, etContains, etRegex, etLike:
      begin
        result := FLookahead.FTerminal;
        Match(FLookahead.FTerminal);
      end;
    etStarts, etEnds:
      begin
        result := FLookahead.FTerminal;
        Match(FLookahead.FTerminal);
        Match(etWITH);
      end
  else
    begin
      ParseError;
      exit(etEq); // dummy to get rid of warning
    end;
  end;

end;

function TParser.MatchValue: variant;
begin
  if FLookahead.FTerminal = etOpenSquareBracket then
  begin
    Match(etOpenSquareBracket);
    exit(MatchValueList);
    Match(etCloseSquareBracket);
  end
  else
  begin
    exit(MatchValueLike);
  end;
end;

function TParser.MatchValueLike: variant;
begin
  case FLookahead.FTerminal of
    etString, etDate, etTime, etID:
      begin
        result := FLookahead.Value;
        Match(FLookahead.Terminal);
      end;
    etNumber:
      begin
        result := strtofloat(FLookahead.Value);
        Match(FLookahead.Terminal);
      end;
  else
    ParseError;
    exit(0); // get rid of warning
  end;
end;

function TParser.MatchValueList: TArray<variant>;
begin
  result := [MatchValue];
  if FLookahead.FTerminal = etComma then
  begin
    Match(etComma);
    result := result + MatchValueList;
  end;
end;

function TParser.Parse(): TExpr;
begin
  result := MatchExpr;
  try
    Match(etEOF);
  except
    result.Free;
    raise;
  end;
end;

procedure TParser.ParseError;
begin
  raise Exception.Create('Parse error');
end;

class function TParser.ParseStr(const AStr: string): string;
var
  LStr: tstringstream;
  LLexer: TLexer;
  lparser: TParser;
  lexpr: TExpr;
begin
  LStr := tstringstream.Create(AStr);
  LLexer := TLexer.Create(LStr);
  lparser := TParser.Create(LLexer);
  try
    lexpr := lparser.Parse();
    try
      exit(lexpr.ExprString);
    finally
      lexpr.Free;
    end;
  finally
    lparser.Free;
  end;
end;

{ TLexer }

constructor TLexer.Create(const AStream: TStream);
begin
  FStream := AStream;
  FEnd := false;
  FReader := TStreamReader.Create(FStream);
end;

function TLexer.CreateSymbol(const ATerminal: TTerminal): TSymbol;
begin
  exit(TSymbol.Create(ATerminal, FValue));
end;

destructor TLexer.Destroy;
begin
  FReader.Free;
  FStream.Free;
  inherited;
end;

function TLexer.GetChar: char;
begin
  if FHasNext then
  begin
    FHasNext := false;
    exit(FNextChar);
  end;
  exit(char(FReader.read));
end;

function TLexer.GetNext: TSymbol;

var
  LChar: char;
  lesc: boolean;
  LStr: string;

  function scanStr(const AChar: char): TSymbol;
  begin
    lesc := false;
    while HasMore do
    begin
      LChar := GetChar;
      if lesc then
      begin
        lesc := false;
        case LChar of
          't':
            FValue := FValue + #9;
        else
          LexerError;
        end;
      end
      else
      begin
        case LChar of
          '''', '"':
            begin
              if AChar <> LChar then
              begin
                FValue := FValue + LChar;
                continue;
              end;
              if (length(FValue) = 5) and (FValue[2] = ':') then
              begin
                exit(CreateSymbol(etTime));
              end;
              if (length(FValue) = 10) and (FValue[5] = '-') and (FValue[8] = '-') then
              begin
                exit(CreateSymbol(etDate));
              end;

              exit(CreateSymbol(etString));
            end;
          '\':
            begin
              lesc := true;
              continue;
            end
        else
          begin
            FValue := FValue + LChar;
          end;
        end;
      end;

    end;
    exit(CreateSymbol(etString));
  end;

  function scanId(): TSymbol;
  begin
    FValue := LChar;
    while HasMore do
    begin
      LChar := GetChar;
      if charinset(LChar, ['a' .. 'z', 'A' .. 'Z', '0' .. '9']) then
      begin
        FValue := FValue + LChar;
      end
      else
      begin
        PushChar(LChar);
        break;
      end;
    end;

    LStr := FValue.ToLower;
    if LStr = 'contains' then
      exit(CreateSymbol(etContains));
    if LStr = 'starts' then
      exit(CreateSymbol(etStarts));
    if LStr = 'ends' then
      exit(CreateSymbol(etEnds));
    if LStr = 'with' then
      exit(CreateSymbol(etWITH));
    if LStr = 'regex' then
      exit(CreateSymbol(etRegex));
    if LStr = 'like' then
      exit(CreateSymbol(etLike));
    if LStr = 'between' then
      exit(CreateSymbol(etBetween));
    if LStr = 'not' then
      exit(CreateSymbol(etNOT));
    if LStr = 'and' then
      exit(CreateSymbol(etAND));
    if LStr = 'or' then
      exit(CreateSymbol(etOR));
    exit(CreateSymbol(etID));
  end;

  function scanNum(): TSymbol;
  begin
    // TODO: actually support double format
    FValue := LChar;
    while HasMore do
    begin
      LChar := GetChar;
      if charinset(LChar, ['0' .. '9']) then
      begin
        FValue := FValue + LChar;
      end
      else
      begin
        PushChar(LChar);
        exit(CreateSymbol(etNumber));
      end;
    end;
    exit(CreateSymbol(etNumber));
  end;

begin
  if FEnd then
    exit(nil);
  FValue := '';
  while HasMore do
  begin
    LChar := GetChar;
    if not(charinset(LChar, [' ', #9])) then
    begin
      PushChar(LChar);
      break;
    end;
  end;
  if not HasMore then
  begin
    FEnd := true;
    exit(CreateSymbol(etEOF));
  end;
  LChar := GetChar;
  case LChar of
    'a' .. 'z', 'A' .. 'Z':
      exit(scanId);
    '"':
      exit(scanStr('"'));
    '''':
      exit(scanStr(''''));
    '0' .. '9':
      exit(scanNum);
    ',':
      exit(CreateSymbol(etNumber));
    '<':
      begin
        if HasMore then
        begin
          LChar := GetChar;
          case LChar of
            '=':
              begin
                exit(CreateSymbol(etLTE));
              end;
            '>':
              begin
                exit(CreateSymbol(etNotEQ));
              end;
          else
            begin
              PushChar(LChar);
            end;
          end;
        end;
        exit(CreateSymbol(etLT));
      end;
    '>':
      begin
        if HasMore then
        begin
          LChar := GetChar;
          case LChar of
            '=':
              begin
                exit(CreateSymbol(etGTE));
              end;
          else
            begin
              PushChar(LChar);
            end;
          end;
        end;
        exit(CreateSymbol(etGT));
      end;
    '!':
      begin
        if HasMore then
        begin
          LChar := GetChar;
          case LChar of
            '=':
              begin
                exit(CreateSymbol(etNotEQ));
              end;
          end;
        end;
        LexerError;
        exit(nil);
      end;
    '(':
      begin
        exit(CreateSymbol(etOpenBracket));
      end;
    ')':
      begin
        exit(CreateSymbol(etCloseBracket));
      end;
    '[':
      begin
        exit(CreateSymbol(etOpenSquareBracket));
      end;
    ']':
      begin
        exit(CreateSymbol(etCloseSquareBracket));
      end;
    '=', ':':
      begin
        exit(CreateSymbol(etEq));
      end;
  else
    begin
      LexerError;
      exit(nil);
    end;
  end;
end;

function TLexer.HasMore: boolean;
begin
  if FEnd then
    exit(false);
  if FHasNext then
    exit(true);
  exit(not FReader.EndOfStream);
end;

procedure TLexer.LexerError;
begin
  raise Exception.Create('Lexer error');
end;

procedure TLexer.PushChar(const AChar: char);
begin
  FNextChar := AChar;
  FHasNext := true;
end;

{ TSymbol }

constructor TSymbol.Create(const ATerminal: TTerminal; const AValue: variant);
begin
  FTerminal := ATerminal;
  FValue := AValue;
end;

{ TBetweenExpr }

constructor TBetweenExpr.Create(const AId: string; const AStart, AEnd: variant; const ANot: boolean);
begin
  inherited Create(etBetween, ANot);
  FId := AId;
  FStart := AStart;
  FEnd := AEnd;
  FNot := ANot;
end;

function TBetweenExpr.GetExprString: string;
begin
  result := FId + ' between ' + FStart + ' and ' + FEnd;

  if FNot then
  begin
    result := 'not (' + result + ')';
  end;

end;

{ TSimpleExpr }

constructor TSimpleExpr.Create(const ATerminal: TTerminal; const AId: string; const AValue: variant; const ANot: boolean);
var
  lterminal: TTerminal;
  LNot: boolean;
begin
  lterminal := ATerminal;
  LNot := ANot;
  if LNot then
  begin
    lterminal := TNegTerminal[ATerminal];
    // if they are the same, we wrap the negation
    if lterminal <> ATerminal then
      LNot := false;
  end;
  inherited Create(lterminal, LNot);
  FId := AId;
  FValue := AValue;
end;

function TSimpleExpr.GetExprString: string;
var
  lquote: boolean;
begin
  result := FId + ' ' + GetTerminalStr(Terminal) + ' ';
  case Terminal of
    etString, etDate, etTime:
      lquote := true;
  else
    lquote := false;
  end;

  if varisstr(FValue) then
    lquote := true;

  if lquote then
    result := result + '''';

  if Terminal in [etEnds, etContains] then
    result := result + '%';

  result := result + vartostr(FValue);

  if Terminal in [etStarts, etContains] then
    result := result + '%';

  if lquote then
    result := result + '''';

  if FNot then
  begin
    result := 'not (' + result + ')';
  end;

end;

{ TTerminalExpr }

constructor TTerminalExpr.Create(const ATerminal: TTerminal; const ANot: boolean);
begin
  inherited Create();
  FNot := ANot;
  FTerminal := ATerminal;
end;

{ TUnaryExpr }

constructor TUnaryExpr.Create(const ATerminal: TTerminal; const AExpr: TExpr);
begin
  inherited Create(ATerminal, false);
  FExpr := AExpr;
end;

destructor TUnaryExpr.Destroy;
begin
  FExpr.Free;
  inherited;
end;

function TUnaryExpr.GetExprString: string;
begin
  result := FExpr.GetExprString;
  if Terminal = etNOT then
  begin
    exit('not (' + result + ')');
  end;
end;

{ TBinaryExpr }

constructor TBinaryExpr.Create(const ATerminal: TTerminal; const ALeftExpr, ARightExpr: TExpr);
begin
  inherited Create(ATerminal, false);
  FLeftExpr := ALeftExpr;
  FRightExpr := ARightExpr;
end;

destructor TBinaryExpr.Destroy;
begin
  FLeftExpr.Free;
  FRightExpr.Free;
  inherited;
end;

function TBinaryExpr.GetExprString: string;

begin
  result := '(' + FLeftExpr.GetExprString + ')' + //
    ' ' + GetTerminalStr(Terminal) + //
    ' (' + FRightExpr.GetExprString + ')';
end;

{ TValueExpr<T> }

constructor TValueExpr<T>.Create(const AValue: T);
begin
  inherited Create();
  FValue := AValue;
end;

{ TIdExpr }

function TIdExpr.GetExprString: string;
begin
  exit(FValue);
end;

{ TStringExpr }

function TStringExpr.GetExprString: string;
begin
  exit('''' + FValue + '''');
end;

{ TNumberExpr }

function TNumberExpr.GetExprString: string;
begin
  exit(floattostr(FValue));
end;

{ TDateExpr }

function TDateExpr.GetExprString: string;
begin
  exit('''' + formatdatetime('yyyy-mm-dd', FValue) + '''');
end;

{ TTimeExpr }

function TTimeExpr.GetExprString: string;
begin
  exit('''' + formatdatetime('hh:mm', FValue) + '''');
end;

end.
