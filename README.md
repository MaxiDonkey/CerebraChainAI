# Asynchronous Promises and Thought Chains for Generative AI in Delphi

Advanced asynchronous management in Delphi using promises, enabling non-blocking execution and converting synchronous functions into asynchronous ones. Facilitates efficient interaction with generative AI models while structuring and mastering thought chains. Optimized for intelligent and scalable workflows.
___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3/11/12-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)
![GitHub](https://img.shields.io/badge/Updated%20on%20february%2014,%202025-blue)

<br/>

- [Introduction](#Introduction)
- [Managing Promises and Thought Chains with Generative AI Models](#Managing-Promises-and-Thought-Chains-with-Generative-AI-Models)
- [Transforming a Synchronous Function into an Asynchronous One](#Transforming-a-Synchronous-Function-into-an-Asynchronous-One)
    - [Getting started](#Getting-started)
    - [I like fruits](#I-like-fruits)
    - [Advantages ](#Advantages )
    - [Prompt and response using JSON](#Prompt-and-response-using-JSON)
- [Contributing](#contributing)
- [License](#license)

<br/>

# Introduction

In the realm of asynchronous development with Delphi, **managing promises and thought chains** is essential for fully leveraging the power of generative AI models. This project provides a flexible and modular solution to efficiently structure one's reasoning (**thought chains**) while interacting with models like **OpenAI, Claude, Gemini, Mistral, GroqCloud, Hugging Face, and Deepseek**.  

The `ASync.Promise` unit integrates seamlessly with existing wrappers:  
- [Anthropic (Claude)](https://github.com/MaxiDonkey/DelphiAnthropic)  
- [Google (Gemini)](https://github.com/MaxiDonkey/DelphiGemini)  
- [Mistral](https://github.com/MaxiDonkey/DelphiMistralAI)  
- [GroqCloud](https://github.com/MaxiDonkey/DelphiGroqCloud)  
- [Hugging Face](https://github.com/MaxiDonkey/DelphiHuggingFace)  
- [Deepseek](https://github.com/MaxiDonkey/DelphiDeepseek)  
- [GenAI for OpenAI](https://github.com/MaxiDonkey/DelphiGenAI)  

Simply adding this unit is **enough** to benefit from optimized promise management, as the asynchronous mechanism is already handled by the `Async.Params` and `Async.Support` units in these projects.  

<br/>

Additionally, **we will explore how to extend a synchronous function into an asynchronous one** using the mechanics provided by the `Async.Params` and `Async.Support` units. This approach does **not** rely on promises but rather on a structured asynchronous framework that enables a smooth transition without requiring a complete code rewrite.  

We will emphasize:  
- **Necessary prerequisites** to enable this mechanism  
- **Step-by-step methodology** for implementation  
- **How it improves scalability and responsiveness**  

<br/>

**In summary, `ASync.Promise` provides promise-based management tailored for generative models, while `Async.Params` and `Async.Support` make it easy to extend synchronous functions into asynchronous ones, enhancing performance and responsiveness in applications.**  

<br/>

# Managing Promises and Thought Chains with Generative AI Models

<br/>

## Getting started

To understand how to effectively manage promises and structure thought chains with generative AI models, we will start with a simple yet concrete example. This exercise will help us grasp the underlying mechanisms and see how promises facilitate clear and structured asynchronous execution.

For this demonstration, we will use the [GenAI wrapper for OpenAI](https://github.com/MaxiDonkey/DelphiGenAI) (DelphiGenAI). This choice allows us to illustrate how promises streamline interactions with large language models, making multi-step reasoning more intuitive and manageable.

The goal of this section is to progressively explore how promises work in the context of generative AI by building a structured thought chain. This will allow us to:

- Efficiently organize sequential AI requests.
- Avoid excessive callback nesting and improve code readability.
- Ensure smooth asynchronous execution without blocking the main thread.

We will implement an example where the AI model selects a random fruit, describes its characteristics, and then suggests a similar fruit. This exercise will demonstrate how promises enable AI-driven reasoning to be chained in an elegant and scalable way.

By the end of this section, you will have a solid understanding of how to use `ASync.Promise` to manage complex AI interactions in a structured and efficient manner.

<br/>

## I like fruits

### Step 1 : The promise method

We design a configurable Promise method to adapt it to each stage of processing while avoiding nested calls, which, although possible, go against the Promise pattern. The main objective is to ensure clear, scalable, and easily maintainable code.

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
> We use the ***GenAI*** for ***OpenAI*** wrapper; therefore, we declare the `GenAI` and `GenAI.Types` units in the uses section.

<br/>

We can observe that this method is asynchronous; it calls the `Client.Chat.AsyncCreate` method.

In the `OnSuccess` section of the **Client.Chat.AsyncCreate** callback, we utilized the `Resolve` method of the promise to indicate that capturing the message content from the **GPT-4o** model's response to our request signifies a successful completion of this step.

```Delphi
Result.OnSuccess :=
  procedure(Sender: TObject; Chat: TChat)
  begin
    Resolve(Chat.Choices[0].Message.Content);
  end;
```

In the OnError section of the Client.Chat.AsyncCreate callback, we utilized the Reject method to capture and propagate the exception.

```Delphi
Result.OnError :=
  procedure(Sender: TObject; ErrorMessage: string)
  begin
    Reject(Exception.Create(ErrorMessage));
  end;
```

<br/>

### Step 2 : Chained methods for structured handling

- &Then<T>: Chains operations to execute after a promise resolves.
- &Catch: Handles errors occurring within a promise chain.

These abstractions enable a structured and reusable design.
Avoid deeply nested callbacks as much as possible, as this facilitates a cleaner approach to asynchronous programming in Delphi.

>[!TIP]
> A TMemo component was placed on a form to display the obtained results.

<br/>

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

**What fascinates me about this mechanism is that it all comes down to a single instruction.**

<br/>

Example Execution
![Preview](https://github.com/MaxiDonkey/CerebraChainAI/blob/main/images/Promise.png?raw=true "Preview")

<br/>

### Advantages 

The use of ***promises*** to interact with a generative model offers several key advantages, particularly in terms of ***asynchronous management, code readability, and structuring successive calls***. Here are the main benefits, illustrated with concrete examples:

- **Building structured thought chains to guide the AI toward more precise responses:** Promises help organize a logical sequence of interactions with the generative model while avoiding excessive callback nesting (callback hell).

- **Facilitating modularity and reusability of AI calls for scalable projects:** Promises allow AI actions to be encapsulated in reusable functions, making the code more modular and easily adaptable to different use cases.

- **Providing an interactive interface where users can progressively refine an AI-generated idea:** An application can take into account successive corrections and dynamically adjust the response accordingly.

<br/>

### Prompt and response using JSON

In this more advanced example, we will build a prompt in multiple stages, ensuring that each step generates a response formatted in JSON. These responses will be progressively compiled and integrated to form a final prompt, which will then be executed. 

This approach transforms a single request into multiple sub-requests that are systematically analyzed. By applying this process across several stages, we enhance the relevance of the final response from the outset.

Here, I will only describe step 1, 2 and final step and the construction of the promise. The rest of the code will be provided along with the test application and the source code.

<br/>

#### Prompts

We will use eight prompts throughout the chained processing.

The first prompt is structured as follows:

```Prompt
%s
To contextualize the request for clarification and to define the question's scope:
- Identify the exact subject and the intended objectives (inform, explain, persuade, etc.).
- Determine the geographical scope, historical period (if applicable), or disciplinary field to avoid any confusion.
- Reasonably broaden the question.

Respond using the following JSON format and only this format and no code container:
{
  "context": {
    "question": "Exact title or statement of the question",
    "objectives": "What is being sought to understand, demonstrate, defend, challenge, analyze, or explain",
    "scope": "Scope or limitations (timeframe, geography, etc.)"
  }
}
```
*Comment:* We will format it to include the question to be addressed on the first line.

<br/>

For the second step, we will use the following prompt:

```Prompt
%s
Based on the previous JSON, break down the question into sub-themes:
- Identify all major areas of analysis (for example, if the question is "What are the impacts of remote work on productivity?", separate the analysis into "Work Organization", "Social Impacts", "Technological Aspects", "Economic Aspects", etc.).
- For each area, formulate more specific sub-questions.

Respond using the following JSON format and only this format and no code container:
{
  "context": {
    "question": "Exact title or wording of the question",
    "objectives": "What we aim to understand, demonstrate, defend, challenge, analyze, or explain",
    "scope": "Scope or limitations (timeframe, geography, etc.)"
  },
  "themes": [
    {
      "theme_name": "Theme name",
      "key_points": [
        "Key idea 1",
        "Key idea 2",
        "Key idea 3"
      ]
    }
    // Repeat for each theme
  ]
}
```
*Comment:* This time, we will format the result from the previous step as JSON by passing it to the prompt.

<br/>

And so on throughout the entire process.

In the final step, we will provide the JSON-formatted result from the penultimate step and request the generation of a well-documented document based on all the collected information.

For the final step, we will use following prompt:

```Prompt
%s
Write an article with a philosophical approach to answer the question from the previous JSON, using the information contained in that same JSON.
- Use an unconventional yet accurate tone to captivate the reader.
- Pay close attention to clarity, relevance, and originality in the writing.
``` 
*Comment:* This time, we will format the result from the previous step as JSON by passing it to the prompt.

<br/>

The prompts for the intermediate steps can be found in the source files, serving as a reference for the demo application.

<br/>

#### The promise

The code used for the promise is as follows:

```Delphi
//uses GenAI, GenAI.Types, ASync.Promise;
function CreateDocCreatorPromise(const Prompt: string; const Developer: string = ''): TPromise<string>;
begin
  var Client := TGenAIFactory.CreateInstance(My_Key);

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
```

>[!NOTE]
> Note that in this example, we use only one type of configurable promise. However, it would be entirely possible to use multiple, each with distinct functionalities—some relying on data from a database, while others leverage your own code. This approach enables seamless integration of various tools, making it easier to build modern applications that utilize artificial intelligence while remaining adaptable to other use cases. It is the ideal combination.

#### The resolution chain

```Delphi
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
      end)
   .&Catch(
     procedure(E: Exception)
     begin
       Display(Memo1, 'Erreur : ' + E.Message);
     end);
```

<br/>

# Transforming a Synchronous Function into an Asynchronous One

<br/>

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.