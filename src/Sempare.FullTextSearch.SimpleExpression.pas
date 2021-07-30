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
unit Sempare.FullTextSearch.SimpleExpression;

interface

uses
  Sempare.FullTextSearch.Expression,
  System.SysUtils,
  System.Classes;

type
  TExpr = class abstract
  public
    function GetExprString(const AField: string): string; virtual; abstract;
  end;

  TValueExpr<T> = class abstract(TExpr)
  private
    FValue: T;
  public
    constructor Create(const AValue: T);
    property Value: T read FValue;
  end;

  TStringExpr = class(TValueExpr<string>)
  public
    function GetExprString(const AField: string): string; override;
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
  public
    constructor Create(const ATerminal: TTerminal; const AExpr: TExpr);
    destructor Destroy; override;
    function GetExprString(const AField: string): string; override;
  end;

  TBinaryExpr = class(TTerminalExpr)
  private
    FLeftExpr: TExpr;
    FRightExpr: TExpr;
  public
    constructor Create(const ATerminal: TTerminal; const ALeftExpr, ARightExpr: TExpr);
    destructor Destroy; override;
    function GetExprString(const AField: string): string; override;
  end;

  TParser = class
  private
    FLexer: TLexer;
    FLookahead: TSymbol;
    function Match(const ATerminal: TTerminal): string;
    function MatchExpr(const ADoOr: boolean = true; const AUseOrAsDefault: boolean = true): TExpr;
    function MatchValueLike: variant;
    procedure ParseError;

  public
    constructor Create(const aLexer: TLexer);
    destructor Destroy; override;

    function Parse(const AUseOrAsDefault: boolean): TExpr;

    class function ParseStr(const AField, AStr: string; const AUseOrAsDefault: boolean = true): string; static;

  end;

implementation

uses
  System.Variants;

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
  if not assigned(FLookahead) or (FLookahead.Terminal <> ATerminal) then
  begin
    ParseError;
  end;
  result := FLookahead.Value;
  FLookahead.Free;
  FLookahead := FLexer.GetNext;
end;

function TParser.MatchExpr(const ADoOr: boolean = true; const AUseOrAsDefault: boolean = true): TExpr;
var
  lvalue: variant;
  lexpr: TExpr;
  LTerm: TTerminal;
begin
  result := nil;
  case FLookahead.Terminal of
    etString:
      begin
        lvalue := MatchValueLike;
        result := TStringExpr.Create(lvalue);
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

  // handle case with no logical operators seperating strings
  while FLookahead.Terminal = etString do
  begin
    lvalue := Match(etString);
    if AUseOrAsDefault then
      LTerm := etOR
    else
      LTerm := etAND;
    result := TBinaryExpr.Create(LTerm, result, TStringExpr.Create(lvalue));
  end;

  if FLookahead.Terminal = etAND then
  begin
    Match(etAND);
    // we indicate that we should not process etOR so that precedence can be maintained
    result := TBinaryExpr.Create(etAND, result, MatchExpr(false));
  end;

  if FLookahead.Terminal = etOR then
  begin
    if ADoOr then
    begin
      Match(etOR);
      result := TBinaryExpr.Create(etOR, result, MatchExpr);
    end;
    exit;
  end;
  if FLookahead.Terminal in [etEOF, etCloseBracket] then
  begin
    exit;
  end;
  ParseError;
end;

function TParser.MatchValueLike: variant;
begin
  case FLookahead.Terminal of
    etString:
      begin
        result := FLookahead.Value;
        Match(FLookahead.Terminal);
      end;
  else
    ParseError;
    exit(0); // get rid of warning
  end;
end;

function TParser.Parse(const AUseOrAsDefault: boolean): TExpr;
begin
  result := MatchExpr(true, AUseOrAsDefault);
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

class function TParser.ParseStr(const AField, AStr: string; const AUseOrAsDefault: boolean): string;
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
    lexpr := lparser.Parse(AUseOrAsDefault);
    try
      exit(lexpr.GetExprString(AField));
    finally
      lexpr.Free;
    end;
  finally
    lparser.Free;
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

function TUnaryExpr.GetExprString(const AField: string): string;
begin
  result := FExpr.GetExprString(AField);
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

function TBinaryExpr.GetExprString(const AField: string): string;

begin
  result := '(' + FLeftExpr.GetExprString(AField) + ')' + //
    ' ' + GetTerminalStr(Terminal) + //
    ' (' + FRightExpr.GetExprString(AField) + ')';
end;

{ TValueExpr<T> }

constructor TValueExpr<T>.Create(const AValue: T);
begin
  inherited Create();
  FValue := AValue;
end;

{ TStringExpr }

function TStringExpr.GetExprString(const AField: string): string;
begin
  // TODO: NOTE!! this is not sql injection safe!
  if FValue.Contains('%') then
    exit(AField + ' like ''' + FValue + '''')
  else
    exit(AField + ' = ''' + FValue + '''');
end;

end.
