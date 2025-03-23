import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Leidangur {

    private static class Node{
        private Character item;
        private Node next;
    }

    private int n;
    private Node first;

    public Leidangur(){
        first = null;
        n = 0;
    }

    public Character getItem(){
        return first.item;
    }

    public Leidangur newStack(){
        Leidangur stack = new Leidangur();
        return stack;
    }

    public boolean isEmpty(){
        return first == null;
    }

    public void push(Character item){
        Node oldFirst = first;
        Node newNode = new Node();
        newNode.item = item;
        newNode.next = oldFirst;
        this.first = newNode;
        n++;
    }

    public Character pop(){
        if(isEmpty()) return 'E';
        Character result = first.item;
        Node oldNode = first;
        first = first.next;
        oldNode.next = null;
        n--;
        return result;
    }



    public static void main(String[] args) throws IOException {

        BufferedReader buf = new BufferedReader(new InputStreamReader(System.in));
        String line = buf.readLine();
        char[] arr = line.toCharArray();
        Leidangur stack = new Leidangur();
        int money = 0;
        int gold = 0;
        int jewels = 0;

        char m = 't';
        for(char c : arr){
            if(c == 'p'){
               stack.push(c);
               money++;
            }
            else if(c == 'g'){
                stack.push(c);
                gold++;
            }
            else if(c == 'o'){
                stack.push(c);
                jewels++;
            }
            else if(c == 'P' || c == 'G' || c == 'O'){
                char check = Character.toLowerCase(c);
                m = 't';
                while(m != check){
                    m = stack.pop();
                    if(m == 'o'){
                        jewels--;
                    }
                    if(m == 'g'){
                        gold--;
                    }
                    if(m == 'p'){
                        money--;
                    }
                    if(m == 'E'){
                        break;
                    }
                }
                if(m == 'E') break;
            }
        }
        if(m == 'E'){
            System.out.println("Neibb");
        }
        else{
            System.out.println(money);
            System.out.println(gold);
            System.out.println(jewels);
        }

        }
}
