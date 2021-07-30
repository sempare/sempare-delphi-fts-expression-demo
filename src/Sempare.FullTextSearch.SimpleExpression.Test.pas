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
unit Sempare.FullTextSearch.SimpleExpression.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TSimpleExprTest = class
  public

    [Test]
    procedure TestSimpleExpr;

    [Test]
    procedure TestWildCardExpr;

    [Test]
    procedure TestWildCard2Expr;

    [Test]
    procedure TestNotExpr;

    [Test]
    procedure TestOrExpr;

    [Test]
    procedure TestPrecidentExpr;

  end;

implementation

uses
  system.Classes,
  system.SysUtils,
  Sempare.FullTextSearch.SimpleExpression;

{ TSimpleExprTest }

procedure TSimpleExprTest.TestSimpleExpr;
begin
  assert.AreEqual('name = ''conrad''', tparser.ParseStr('name', '"conrad"'));
end;

procedure TSimpleExprTest.TestWildCard2Expr;
begin
  assert.AreEqual('(name like ''conrad%'') or (name like ''%pete'')', tparser.ParseStr('name', '"conrad%" "%pete"'));
end;

procedure TSimpleExprTest.TestWildCardExpr;
begin
  assert.AreEqual('name like ''conrad%''', tparser.ParseStr('name', '"conrad%"'));

end;

procedure TSimpleExprTest.TestNotExpr;
begin
  assert.AreEqual('not (name = ''conrad'')', tparser.ParseStr('name', 'not "conrad"'));
end;

procedure TSimpleExprTest.TestOrExpr;
begin
  assert.AreEqual('(name = ''abc'') or (name = ''def'')', tparser.ParseStr('name', '"abc" or "def"'));
end;

procedure TSimpleExprTest.TestPrecidentExpr;
begin
  assert.AreEqual('(name = ''abc'') and (not ((name = ''321'') or (name = ''def'')))', tparser.ParseStr('name', '''abc'' and  not ''321'' or  ''def'''));
end;

initialization

TDUnitX.RegisterTestFixture(TSimpleExprTest);

end.
