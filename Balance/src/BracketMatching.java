import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class BracketMatching{

    private char[] stack;
    private int n;

    public BracketMatching(){
        stack = new char[1];
        n = 0;
    }

    public void push(char c){
        if(n==stack.length){
            char[] newStack = new char[stack.length*2];
            for(int i=0 ; i<n ; i++){
                newStack[i] = stack[i];
            }
            stack = newStack;
        }
        stack[n++] = c;
    }

    public char peek(){
        if(isEmpty()) return 'w';
        return stack[n-1];
    }

    public char pop(){
        if(isEmpty()) return 'w';
        char result = stack[--n];
        if(n>0 && n==stack.length/4){
            int newsize = stack.length/2;
            char[] newStack = new char[newsize];
            for(int i=0 ; i<n ; i++){
                newStack[i] = stack[i];
            }
            stack = newStack;
        }
        return result;
    }

    public boolean isEmpty(){
        if(n==0) return true;
        return false;
    }


    public static void main(String[] args) throws IOException {
        BufferedReader buf = new BufferedReader(new InputStreamReader(System.in));
        BracketMatching bal = new BracketMatching();
        String lineFirst = buf.readLine();
        String line = buf.readLine();

        char[] tokens = line.toCharArray();
        String result = "Invalid";
        for(char c : tokens){
            if(c == '[' ||c == '(' || c == '{'  ){
                bal.push(c);
            }
            if(c == ']'){
                if(bal.peek() != '['){
                    result = "Invalid";
                    break;
                }
                else if (bal.peek() == '['){
                    bal.pop();
                    result = "Valid";
                }
            }
            if(c == ')'){
                if(bal.peek() != '('){
                    result = "Invalid";
                    break;
                }
                else if (bal.peek() == '('){
                    bal.pop();
                    result = "Valid";
                }
            }
            if(c == '}'){
                if(bal.peek() != '{'){
                    result = "Invalid";
                    break;
                }
                else if (bal.peek() == '{'){
                    bal.pop();
                    result = "Valid";
                }
            }
        }
        if(bal.isEmpty()) System.out.println(result);
        if(!bal.isEmpty()) System.out.println("Invalid");

    }
}