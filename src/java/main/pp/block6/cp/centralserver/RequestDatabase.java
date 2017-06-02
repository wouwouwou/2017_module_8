package pp.block6.cp.centralserver;

import pp.block6.cp.Message;

/*
 * Well, wouldn't it be useful if you could just prefetch all URLs which are given here?
 * Therefore this is just the most basic (non)-functionality hence it works but it doesn't.
 * 
 * For the final test this class will be updated to include the proper code.
 */

public class RequestDatabase {

    public boolean verifyResponse(Message response) {
        // TODO Auto-generated method stub
        return true;
    }

    public Message getRequest() {
        return new Message("1234","http://google.com");
    }


}
