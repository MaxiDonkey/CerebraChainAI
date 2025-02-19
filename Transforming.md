#Transforming a Synchronous Function into an Asynchronous One

- [Synchronous method](#Synchronous-method)
- [Asynchronous method](#Asynchronous-method)
- [Use case](#Use-case)

## Synchronous method

The mechanism for extending a synchronous function into an asynchronous one requires that the functions you create must return a class instance. This prerequisite ensures proper handling within the asynchronous framework.

```Delphi
  type
     TResultFunction = class
     private
       ...
     public
      ...
     end;
```

The class name is irrelevant, regardless of whether it is a base or derived class.

Next, we declare a `DoSomething` method of type function, regardless of whether it includes parameters.
All possible scenarios are considered.

```Delphi
  TMyClass = class
  private
   public
    function DoSomething(const Value: string): TResultFunction;
   end;
```

>[!WARNING]
> It is essential that DoSomething returns a class instance.

All the necessary conditions are met to extend DoSomething into an asynchronous method.

## Asynchronous method

To achieve this, ensure that the `Async.Support` unit is included in the uses section. Then, declare a new type based on the generic `TAsynCallBack<T>`. The generic parameter `T` must now be resolved and will naturally be substituted by the `TResultFunction` class.

```Delphi
//uses Async.Support;

   type
    TAsynResultFunction = TAsynCallBack<TResultFunction>;
```

We can now declare the asynchronous method `AsynDoSomething` within `TMyClass`.

Although it is possible to use the same name as the synchronous method and apply the `overload` directive, this may present challenges when generating XML documentation. In particular, XML documentation tools often struggle to correctly differentiate between overloaded methods, frequently applying the same contextual help message to both. This can be problematic if each method has distinct comments tailored to its specific behavior.

To avoid any ambiguity, we will adopt a cautious approach in this example by giving the asynchronous method a distinct name, prefixing it with `Asyn` to clearly differentiate it from its synchronous counterpart.

```Delphi
TMyClass = class
  private
   public
    function DoSomething(const Value: string): TResultFunction;
    procedure AsynDoSomething(const Value: string; CallBacks: TFunc<TAsynResultFunction>);
   end;
```

The `CallBacks` parameter is a lambda function returning the `TAsynResultFunction` type, which we previously defined.

The implementation of `AsynDoSomething`, like all methods extended to support asynchronous execution, will generally follow this structure:

```Delphi
procedure TMyClass.AsynDoSomething(const Value: string; CallBacks: TFunc<TAsynResultFunction>);
begin
  with TAsynCallBackExec<TAsynResultFunction, TResultFunction>.Create(CallBacks) do //variable elements (TAsynResultFunction, TResultFunction)
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TResultFunction  //variable element
      begin
        Result := Self.DoSomething(Value); //Call the synchronous method here: variable element
      end);
  finally
    Free;
  end;
end;
```

I have marked the sections that may vary between asynchronous methods with the label "variable element." As you can see, very few elements actually change.

This concludes this section; everything is now in place.

## Use case

Now, let's look at how this method is called.

*MyClass is an instance of the TMyClass class.*
```Delphi
  MyClass.AsynDoSomething(
          ParamString,
          function : TAsynResultFunction
          begin
             Result.sender := nil; //or an instance
             Result.OnStart :=
               procedure (Sender: TObject)
               begin

               end;
             Result.OnSuccess :=
               procedure (Sender: TObject; Value: TResultFunction)  // if Result.sender is not nil, then the Sender parameter of this procedure 
               begin                                                // will be assigned the value of Result.sender.                 

               end;
             Result.OnError :=
               procedure (Sender: TObject; ErrorMessage: string)
               begin

               end
          end);
```

We have thoroughly explored how to extend a synchronous method into an asynchronous one in a simple and efficient manner.

You now have the flexibility to enhance promises by incorporating your own custom asynchronous methods.
