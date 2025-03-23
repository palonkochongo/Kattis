import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class PairingSocks {

    private int[] stack;
    private int n;


    public PairingSocks(int size){
        stack = new int[size];
        n = 0;
    }

    public boolean isEmpty(){
        return n==0;
    }

    public int peek(){
       if(isEmpty()) return -1;
       return stack[n-1];
    }

    public void resize(int i){
        int[] newStack = new int[i];
        for(int j=0 ; j<n ; j++){
            newStack[j] = stack[j];
        }
        stack = newStack;
    }

    public int pop(){
        int result = stack[--n];
        if(n>0 && n == stack.length/4){
            resize(stack.length/2);
        }
        return result;
    }

    public void push(int i){
        if(n==stack.length){
            resize(stack.length*2);
        }
        stack[n++] = i;
    }

    public static void main(String[] args) throws IOException {

        BufferedReader buf = new BufferedReader(new InputStreamReader(System.in));
        String line = buf.readLine();
        String line2 = buf.readLine();
        String[] withoutSpaces = line2.split(" ");

        PairingSocks originStack = new PairingSocks(Integer.parseInt(line)*2);
        PairingSocks auxStack = new PairingSocks(1);


        for(int i=withoutSpaces.length-1 ; i>=0 ; i--){
            originStack.push(Integer.parseInt(withoutSpaces[i]));
        }

        int moves = 0;
        while(!originStack.isEmpty()){
            auxStack.push(originStack.pop());
            moves++;
            while(!auxStack.isEmpty() &&
                    !originStack.isEmpty() &&
                    auxStack.peek() == originStack.peek())
            {
                    auxStack.pop();
                    originStack.pop();
                    moves++;
            }
        }
        if(auxStack.isEmpty()) {
            System.out.println(moves);
        }
        else{
            System.out.println("impossible");
        }
    }
}