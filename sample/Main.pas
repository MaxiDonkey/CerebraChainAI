unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.JSON,
  GenAI, GenAI.Types, ASync.Promise, Async.Support;

(*******************************************************************************

    Verify the search paths for accessing the source directory and the directory
    containing GenAI.

    If not found, GenAI can be downloaded from the following address:
    https://github.com/MaxiDonkey/DelphiGenAI

*******************************************************************************)

const
  My_Key = 'YOUR_OPENAI_KEY';
  PATH = '..\..\..\prompts\';


type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Memo2: TMemo;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FCancel: Boolean;
    function GetFileContent(const FileName: string): string;
    function PromptStep1: string;
    function PromptStep(const FileName: string; const PriorResult: string): string;
  public
    Client: IGenAI;
    procedure WriteDocumentStart;
    procedure WriteDocumentEnd;
    property Cancel: Boolean read FCancel write FCancel;
  end;

var
  Form1: TForm1;
  LastChoice: string = 'cherry';

implementation

{$R *.dfm}

procedure Display(Sender: TObject; Value: string);
var
  M: TMemo;
begin
  if Sender is TMemo then
    M := TMemo(Sender) else
    M := (Sender as TForm1).Memo1;

  var S := Value.Split([#10]);
  if System.Length(S) = 0 then
    begin
      M.Lines.Add(Value)
    end
  else
    begin
      for var Item in S do
        M.Lines.Add(Item);
    end;

  M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure DisplayStream(Sender: TObject; Value: string); overload;
var
  M   : TMemo;
  Txt : string;
begin
  if Value.IsEmpty then Exit;

  if Sender is TMemo then
    M := TMemo(Sender)
  else
    M := Form1.Memo1;

  Txt := StringReplace(Value, '\n', sLineBreak, [rfReplaceAll]);
  Txt := StringReplace(Txt, #10,  sLineBreak, [rfReplaceAll]);

  M.Lines.BeginUpdate;
  try
    M.SelStart   := M.GetTextLen;
    M.SelLength  := 0;
    M.SelText    := Txt;
  finally
    M.Lines.EndUpdate;
  end;

  M.Perform(EM_SCROLLCARET, 0, 0);
end;

function DoCancellation: Boolean;
begin
  Result := Form1.Cancel;
end;

procedure DisplayStream(Sender: TObject; Value: TChat); overload;
begin
  if Assigned(Value) then
    begin
      for var Item in Value.Choices do
        begin
          DisplayStream(Sender, Item.Delta.Content);
        end;
    end;
end;

function CreateChatPromise(const Prompt: string): TPromise<string>;
begin
//  var Client := TGenAIFactory.CreateInstance(My_Key);
//  Client.API.HttpClient.SendTimeOut := 120000;
//  Client.API.HttpClient.ConnectionTimeout := 120000;

  //non streamed
  Result := TPromise<string>.Create(
    procedure(Resolve: TProc<string>; Reject: TProc<Exception>)
    begin
      Form1.Client.Chat.AsynCreate(
        procedure(Params: TChatParams)
        begin
          Params.Model('gpt-4o-mini');
          Params.Messages([
            FromUser(Prompt)
          ]);
        end,
        function: TAsynChat
        begin
          Result.OnSuccess :=
            procedure(Sender: TObject; Chat: TChat)
            begin
              Resolve(Chat.Choices[0].Message.Content);
            end;
          Result.OnError :=
            procedure(Sender: TObject; ErrorMessage: string)
            begin
              Reject(Exception.Create(ErrorMessage));
            end;
        end);
    end);
end;

function CreateChatStreamPromise(const Prompt: string): TPromise<string>;
var
  buffer: string;
begin
  //streamed
  Result := TPromise<string>.Create(
    procedure(Resolve: TProc<string>; Reject: TProc<Exception>)
    begin
      Form1.Client.Chat.AsynCreateStream(
        procedure (Params: TChatParams)
        begin
          Params.Model('gpt-4o-mini');
          Params.Messages([
            FromUser(Prompt)
          ]);
          Params.Stream;
        end,
        function : TAsynChatStream
        begin
          Result.Sender := Form1.Memo1;

          Result.OnStart := nil;

          Result.OnProgress :=
            procedure (Sender: TObject; Chat: TChat)
            begin
              DisplayStream(Sender, Chat);
              Buffer := Buffer + Chat.Choices[0].Delta.Content;
            end;

          Result.OnSuccess :=
            procedure (Sender: TObject)
            begin
              Resolve(Buffer + sLineBreak);
            end;

          Result.OnError :=
            procedure (Sender: TObject; Error: string)
            begin
              Reject(Exception.Create(Error));
            end;

          Result.OnDoCancel := DoCancellation;

          Result.OnCancellation :=
            procedure (Sender: TObject)
            begin
              Reject(Exception.Create('Aborted'));
            end;

        end)
    end);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  var Prompt1 := 'From the array ["apple", "banana", "orange", "tomato", "nut", "tangerine", "pear", "cherry"], pick a random item. Always respond with ONE word. You can''t choice %s';

  var Prompt2 := 'Indicate with a short sentence the characteristics of the fruit. : %s.';

  var Prompt3 := 'Name another fruit that resembles : %s';

  Memo1.Lines.Add('>>>>>> New attempt');

  CreateChatPromise(Format(Prompt1, [LastChoice]))
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, 'Step 1: '#10 + Value + sLineBreak);
        Result := Value;
        LastChoice := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatPromise(Format(Prompt2, [Value]));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Result := Value;
        Display(Memo1, 'Step 2: '#10 + Value + sLineBreak);
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
        {--- We return the new promise directly without nesting the code }
        Result := CreateChatPromise(Format(Prompt3, [Value]));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Result := Value;
        Display(Memo1, 'Step 3: '#10 + Value + sLineBreak);
        Display(Memo1, sLineBreak);
      end)
   .&Catch(
     procedure(E: Exception)
     begin
       Display(Memo1, 'Erreur : ' + E.Message);
     end);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  WriteDocumentStart;

  {--- "pyramid of doom" !! Need to process with pipeline, but here it's just an example. }
  CreateChatStreamPromise(PromptStep1)
   .&Then<string>(
      function(Value: string): string
      begin
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatStreamPromise(PromptStep('step2.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, sLineBreak + 'Define a response (or data) structure' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatStreamPromise(PromptStep('step3.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, sLineBreak + 'Identify relevant sources (articles, studies, official reports, reference books).' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatStreamPromise(PromptStep('step4.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, sLineBreak + 'Analyze and synthesize the information' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatStreamPromise(PromptStep('step5.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, sLineBreak + 'Formulate a comprehensive and coherent response' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatStreamPromise(PromptStep('step6.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, sLineBreak + 'Reserve a section for the conclusion and perspectives' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatStreamPromise(PromptStep('step7.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, sLineBreak + 'Write an article with a philosophical approach' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatStreamPromise(PromptStep('finalstep.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Result := Value;
        WriteDocumentEnd;
      end)
   .&Catch(
     procedure(E: Exception)
     begin
       Display(Memo1, 'Erreur : ' + E.Message);
       WriteDocumentEnd;
     end);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Form1.Cancel := True;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not HttpMonitoring.IsBusy;
  if not CanClose then
    MessageDLG(
      'Requests are still in progress. Please wait for them to complete before closing the application."',
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Client := TGenAIFactory.CreateInstance(My_Key);
  Client.API.HttpClient.SendTimeOut := 180000;
  Client.API.HttpClient.ConnectionTimeout := 180000;
end;

function TForm1.GetFileContent(const FileName: string): string;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('%s File not found', [FileName]);
  with TStringList.Create do
  try
    LoadFromFile(FileName, TEncoding.UTF8);
    Result := Text;
  finally
    Free;
  end;
end;

function TForm1.PromptStep1: string;
begin
  Result := Memo2.Text;
  if Result.Trim.IsEmpty then
    raise Exception.Create('Formulate a question to address a topic.');

  Result := Format(GetFileContent(PATH + 'step1.txt'), [Result]);
end;

procedure TForm1.WriteDocumentEnd;
begin
  Button2.Caption := 'Create document';
  Button4.Visible := False;
end;

procedure TForm1.WriteDocumentStart;
begin
  Button2.Caption := 'Please wait...';
  Button4.Visible := True;
  Form1.Cancel := False;
  Display(Memo1, 'Contextualize the request for clarification' + sLineBreak);
end;

function TForm1.PromptStep(const FileName: string; const PriorResult: string): string;
begin
  Result := Format(GetFileContent(PATH + FileName), [PriorResult]);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
end.
