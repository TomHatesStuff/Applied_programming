//
//  ContentView.swift
//  Flashcards
//
//  Created by Luke Marshall on 3/9/24.
//
import SwiftUI

struct Flashcard: Codable, Identifiable {
    var id = UUID()
    var question: String
    var answer: String
}

struct TestView: View {
    var flashcards: [Flashcard]

    var body: some View {
        List(flashcards) { flashcard in
            VStack(alignment: .leading) {
                Text("Question: \(flashcard.question)")
                Text("Answer: \(flashcard.answer)")
            }
            .padding()
        }
    }
}

struct ContentView: View {
    @State private var question = ""
    @State private var answer = ""
    @State private var flashcards: [Flashcard] = []
    @State private var showingAnswer = false
    @State private var currentFlashcardIndex = 0
    @State private var showTestView = false

    var body: some View {
        VStack {
            if flashcards.isEmpty {
                Text("No flashcards yet. Add some below!")
            } else {
                Text(showingAnswer ? flashcards[currentFlashcardIndex].answer : flashcards[currentFlashcardIndex].question)
                    .font(.title)
                    .padding()
            }

            if !flashcards.isEmpty {
                Button(action: {
                    if self.showingAnswer {
                        self.currentFlashcardIndex = (self.currentFlashcardIndex + 1) % self.flashcards.count
                    }
                    self.showingAnswer.toggle()
                }) {
                    Text(showingAnswer ? "Next Flashcard" : "Show Answer")
                        .padding()
                        .foregroundColor(.white)
                        .cornerRadius(10)
                }
            }

            HStack {
                TextField("Enter question", text: $question)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .padding()
                TextField("Enter answer", text: $answer)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .padding()
            }

            Button(action: {
                self.flashcards.append(Flashcard(question: self.question, answer: self.answer))
                self.saveFlashcards()
                self.question = ""
                self.answer = ""
            }) {
                Text("Add Flashcard")
                    .padding()
                    .foregroundColor(.white)
                    .cornerRadius(10)
            }

            if !flashcards.isEmpty {
                Button(action: {
                    self.showTestView = true
                }) {
                    Text("view current flashcards")
                        .padding()
                        .foregroundColor(.white)
                        .cornerRadius(10)
                }
                .sheet(isPresented: $showTestView) {
                    TestView(flashcards: self.flashcards)
                }
            }
        }
        .padding()
        .onAppear {
            self.loadFlashcards()
        }
    }

    func saveFlashcards() {
        if let encoded = try? JSONEncoder().encode(flashcards) {
            UserDefaults.standard.set(encoded, forKey: "flashcards")
        }
    }

    func loadFlashcards() {
        if let data = UserDefaults.standard.data(forKey: "flashcards") {
            if let decoded = try? JSONDecoder().decode([Flashcard].self, from: data) {
                flashcards = decoded
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
