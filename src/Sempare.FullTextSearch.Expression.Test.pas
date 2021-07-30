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
unit Sempare.FullTextSearch.Expression.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TExprTest = class
  public

    [Test]
    procedure TestEqExpr;

    [Test]
    procedure TestNotEqExpr;

    [Test]
    procedure TestLtExpr;

    [Test]
    procedure TestLteExpr;

    [Test]
    procedure TestgtExpr;

    [Test]
    procedure TestgteExpr;

    [Test]
    procedure TestContainsExpr;

    [Test]
    procedure TestLikeExpr;

    [Test]
    procedure TestStartsWithExpr;

    [Test]
    procedure TestEndsWithExpr;

    [Test]
    procedure TestNotExpr;

    [Test]
    procedure TestAndExpr;

    [Test]
    procedure TestOrExpr;

    [Test]
    procedure TestPrecidentExpr;

  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  Sempare.FullTextSearch.Expression;

{ TExprTest }

procedure TExprTest.TestAndExpr;
begin
  assert.Areequal('(name = ''abc'') and (text = ''def'')', tparser.ParseStr('name = ''abc'' and text = ''def'''));

end;

procedure TExprTest.TestContainsExpr;
begin

  assert.Areequal('name like ''%abc%''', tparser.ParseStr('name contains "abc"'));

end;

procedure TExprTest.TestEndsWithExpr;
begin
  assert.Areequal('name like ''%abc''', tparser.ParseStr('name ends with "abc"'));

end;

procedure TExprTest.TestEqExpr;
begin
  assert.Areequal('name = ''conrad''', tparser.ParseStr('name="conrad"'));
end;

procedure TExprTest.TestgteExpr;
begin
  assert.Areequal('name >= 10', tparser.ParseStr('name >= 10'));

end;

procedure TExprTest.TestgtExpr;
begin
  assert.Areequal('name > 10', tparser.ParseStr('name > 10'));

end;

procedure TExprTest.TestLikeExpr;
begin
  assert.Areequal('name like ''%abc%''', tparser.ParseStr('name like "%abc%"'));

end;

procedure TExprTest.TestLteExpr;
begin
  assert.Areequal('name <= 10', tparser.ParseStr('name <= 10'));

end;

procedure TExprTest.TestLtExpr;
begin
  assert.Areequal('name < 10', tparser.ParseStr('name < 10'));

end;

procedure TExprTest.TestNotEqExpr;
begin
  assert.Areequal('name != ''conrad''', tparser.ParseStr('name<>"conrad"'));
end;

procedure TExprTest.TestNotExpr;
begin
  assert.Areequal('not (name != ''conrad'')', tparser.ParseStr('not (name!="conrad")'));
end;

procedure TExprTest.TestOrExpr;
begin
  assert.Areequal('(name = ''abc'') or (text = ''def'')', tparser.ParseStr('name = ''abc'' or text = ''def'''));
end;

procedure TExprTest.TestPrecidentExpr;
begin
  assert.Areequal('((name = ''abc'') and (value = ''321'')) or (text = ''def'')', //
    tparser.ParseStr('name = ''abc'' and value = ''321'' or text = ''def'''));
end;

procedure TExprTest.TestStartsWithExpr;
begin
  assert.Areequal('name like ''abc%''', tparser.ParseStr('name starts with "abc"'));
end;

initialization

TDUnitX.RegisterTestFixture(TExprTest);

end.
