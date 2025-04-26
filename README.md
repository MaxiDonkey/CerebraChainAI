# Asynchronous Promises and Thought Chains for Generative AI in Delphi

Advanced asynchronous management in Delphi using promises, enabling non-blocking execution and converting synchronous functions into asynchronous ones. Facilitates efficient interaction with generative AI models while structuring and mastering thought chains. Optimized for intelligent and scalable workflows.
___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3/11/12-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)
![GitHub](https://img.shields.io/badge/Updated%20on%20april%20224,%202025-blue)

<br>

- [Documentation Overview](#documentation-overview)
- [Promise pattern](#promise-pattern)
    - [Why](#why)
    - [How does it work](#how-does-it-work)
    - [Detailed explanations](#detailed-explanations)
- [Transforming a Synchronous method into an Asynchronous One](#transforming-a-synchronous-method-into-an-asynchronous-one)
- [To go further](#to-go-further)
- [Contributing](#contributing)
- [License](#license)

<br>

# Documentation Overview

Comprehensive Project Documentation Reference
- [Transforming a Synchronous Function into an Asynchronous One](https://github.com/MaxiDonkey/CerebraChainAI/blob/main/Transforming.md)
- [Managing Promises and Thought Chains with Generative AI Models](https://github.com/MaxiDonkey/CerebraChainAI/blob/main/ChainOfThought.md)

<br>

# Promise pattern

## WHY ?

To ensure controlled and orderly execution of a series of asynchronous methods while enabling comprehensive processing of intermediate results without causing any blocking.

When applied to asynchronous processing provided by the following wrappers, this approach enables the structuring of thought trees, thereby providing reasoning capabilities to models that lack them.
- [Anthropic (Claude)](https://github.com/MaxiDonkey/DelphiAnthropic)  
- [Google (Gemini)](https://github.com/MaxiDonkey/DelphiGemini)  
- [Mistral](https://github.com/MaxiDonkey/DelphiMistralAI)  
- [GroqCloud](https://github.com/MaxiDonkey/DelphiGroqCloud)  
- [Hugging Face](https://github.com/MaxiDonkey/DelphiHuggingFace)  
- [Deepseek](https://github.com/MaxiDonkey/DelphiDeepseek)  
- [GenAI for OpenAI](https://github.com/MaxiDonkey/DelphiGenAI) 

Furthermore, it offers the flexibility to control or dynamically adapt a new thought framework, which is not possible with a model that has a predefined reasoning structure.

<br>

## How does it work?

We use the Promise design pattern, which enables efficient management of a series of asynchronous requests. This pattern offers several key benefits, including:
- Sequential execution of processes in successive steps.
- Centralized handling of intermediate results.
- A non-blocking operation, ensuring smooth workflow execution.

Refer to [detail explanations ](https://github.com/MaxiDonkey/CerebraChainAI/blob/main/ChainOfThought.md)

Here is a diagram illustrating the Promise pattern.
![Preview](https://github.com/MaxiDonkey/CerebraChainAI/blob/main/images/Schema01.png?raw=true "Preview")

<br>

## Use case example

### The promise

```Delphi
//uses GenAI, GenAI.Types, ASync.Promise;

function CreateChatPromise(const Prompt: string): TPromise<string>;
begin
  var Client := TGenAIFactory.CreateInstance(My_Key);

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
```
>[!NOTE]
> We use the ***[GenAI](https://github.com/MaxiDonkey/DelphiGenAI)*** for ***OpenAI*** wrapper; therefore, we declare the `GenAI` and `GenAI.Types` units in the uses section.

<br>

### The chain of thought

```Delphi
//uses GenAI, GenAI.Types, ASync.Promise;

  LastChoice: string = 'cherry'; //The gpt-4o model does not handle randomness very well, so to avoid repeating the same choice consecutively

  var Prompt1 := 'From the array ["apple", "banana", "orange", "tomato", "nut", "tangerine", "pear", "cherry"], pick a random item. Always respond with ONE word. You can''t choice %s';

  var Prompt2 := 'Indicate with a short sentence the characteristics of the fruit. : %s.';

  var Prompt3 := 'Name another fruit that resembles : %s';

  Memo1.Lines.Add('>>>>>> New attempt');

 
  CreateChatPromise(Format(Prompt1, [LastChoice]))                 //Create the promise
   .&Then<string>(
      function(Value: string): string
      begin
        Memo1.Lines.Add('Step 1: ' + Value);
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
        Memo1.Lines.Add('Step 2: ' + Value);
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
        Memo1.Lines.Add('Step 3: ' + Value);
        Memo1.Lines.Add(sLineBreak);
      end)
   .&Catch(                                                        //Catch error
     procedure(E: Exception)
     begin
       Memo1.Lines.Add('Erreur : ' + E.Message);
     end); 
```
>[!WARNING]
> Execution is asynchronous. It is crucial to ensure that the chained instruction is constructed as a single, continuous statement to avoid introducing intermediate processing steps, which would inherently be synchronous.

<br>

## Detailed explanations

Refer to [part I](https://github.com/MaxiDonkey/CerebraChainAI/blob/main/ChainOfThought.md)

<br>

# Transforming a Synchronous method into an Asynchronous One

Refer to [part II](https://github.com/MaxiDonkey/CerebraChainAI/blob/main/Transforming.md)

<br>

# To go further

To go further: Check out the [SynkFlowAI](https://github.com/MaxiDonkey/SynkFlowAI) project – a proof-of-concept async pipeline framework in Delphi designed to demonstrate modern AI orchestration with Promises, streaming, and chained execution.

<br>

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.