unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.JSON,
  GenAI, GenAI.Types, ASync.Promise;

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
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    function GetFileContent(const FileName: string): string;
    function PromptStep1: string;
    function PromptStep(const FileName: string; const PriorResult: string): string;
  public
    Client: IGenAI;
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

function CreateChatPromise(const Prompt: string): TPromise<string>;
begin
  var Client := TGenAIFactory.CreateInstance(My_Key);
  Client.API.HttpClient.SendTimeOut := 120000;
  Client.API.HttpClient.ConnectionTimeout := 120000;

  Result := TPromise<string>.Create(
    procedure(Resolve: TProc<string>; Reject: TProc<Exception>)
    begin
      Client.Chat.AsynCreate(
        procedure(Params: TChatParams)
        begin
          Params.Model('gpt-4o');
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

function CreateDocCreatorPromise(const Prompt: string; const Developer: string = ''): TPromise<string>;
begin
  var Client := TGenAIFactory.CreateInstance(My_Key);
  Client.API.HttpClient.SendTimeOut := 120000;
  Client.API.HttpClient.ConnectionTimeout := 120000;

  Result := TPromise<string>.Create(
    procedure(Resolve: TProc<string>; Reject: TProc<Exception>)
    begin
      var Messages := TJSONArray.Create;

      if not Developer.Trim.IsEmpty then
        Messages.Add(FromDeveloper(Developer).Detach);

      if Prompt.Trim.IsEmpty then
        raise Exception.Create('Prompt can''t be null');
      Messages.Add(FromUser(Prompt).Detach);

      Client.Chat.AsynCreate(
        procedure(Params: TChatParams)
        begin
          Params.Model('gpt-4o');
          Params.Messages(Messages);
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
        Display(Memo1, 'Step 1: ' + Value);
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
        Display(Memo1, 'Step 2: ' + Value);
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
        Display(Memo1, 'Step 3: ' + Value);
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

  Button2.Caption := 'Please wait...';
  CreateChatPromise(PromptStep1)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, 'Contextualize the request for clarification' + sLineBreak + Value);
        Display(Memo1, sLineBreak + 'Identify all major areas of analysis' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatPromise(PromptStep('step2.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, Value + sLineBreak);
        Display(Memo1, sLineBreak + 'Define a response (or data) structure' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatPromise(PromptStep('step3.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, Value + sLineBreak);
        Display(Memo1, sLineBreak + 'Identify relevant sources (articles, studies, official reports, reference books).' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatPromise(PromptStep('step4.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, Value + sLineBreak);
        Display(Memo1, sLineBreak + 'Analyze and synthesize the information' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatPromise(PromptStep('step5.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, Value + sLineBreak);
        Display(Memo1, sLineBreak + 'Formulate a comprehensive and coherent response' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatPromise(PromptStep('step6.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, Value + sLineBreak);
        Display(Memo1, sLineBreak + 'Reserve a section for the conclusion and perspectives' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatPromise(PromptStep('step7.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, Value + sLineBreak);
        Display(Memo1, sLineBreak + 'Write an article with a philosophical approach' + sLineBreak);
        Result := Value;
      end)
   .&Then(
     function(Value: string): TPromise<string>
     begin
       {--- We return the new promise directly without nesting the code }
       Result := CreateChatPromise(PromptStep('finalstep.txt', Value));
     end)
   .&Then<string>(
      function(Value: string): string
      begin
        Display(Memo1, Value + sLineBreak);
        Result := Value;
        Button2.Caption := 'Create document';
      end)
   .&Catch(
     procedure(E: Exception)
     begin
       Display(Memo1, 'Erreur : ' + E.Message);
       Button2.Caption := 'Create document';
     end);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Client := TGenAIFactory.CreateInstance(My_Key);
  Client.API.HttpClient.SendTimeOut := 120000;
  Client.API.HttpClient.ConnectionTimeout := 120000;
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

function TForm1.PromptStep(const FileName: string; const PriorResult: string): string;
begin
  Result := Format(GetFileContent(PATH + FileName), [PriorResult]);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
end.