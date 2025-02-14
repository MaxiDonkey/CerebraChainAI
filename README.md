# Asynchronous Promises and Thought Chains for Generative AI in Delphi

Advanced asynchronous management in Delphi using promises, enabling non-blocking execution and converting synchronous functions into asynchronous ones. Facilitates efficient interaction with generative AI models while structuring and mastering thought chains. Optimized for intelligent and scalable workflows.
___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3/11/12-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)
![GitHub](https://img.shields.io/badge/Updated%20on%20february%2014,%202025-blue)

<br/>

- [Introduction](#Introduction)
- [Contributing](#contributing)
- [License](#license)

<br/>

# Introduction

In the realm of asynchronous development with Delphi, **managing promises and thought chains** is essential for fully leveraging the power of generative AI models. This project provides a flexible and modular solution to efficiently structure one's reasoning (**thought chains**) while interacting with models like **OpenAI, Claude, Gemini, Mistral, GroqCloud, Hugging Face, and Deepseek**.  

The **ASync.Promise** unit integrates seamlessly with existing wrappers:  
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

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.