//Jasper 2016_12_6
package org.openqa.selenium.example;

import org.openqa.selenium.*;
import org.openqa.selenium.chrome.ChromeDriver;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

//--time_out

//account, thread: interface
//start from sec lines

public class Main  {
    public static void main(String[] urls){
        long startTime = System.currentTimeMillis();

        String[] sss2 = {"069.ddns.us"};

        //Integer num_threads;
        //account

        test_urls tests = new test_urls(sss2);
        tests.set_threads_counter(new Integer(3));
        tests.call();

        long estimatedTime = System.currentTimeMillis() - startTime;
        System.out.println(estimatedTime/1000);
    }
}

class test_urls{
    ThreadsCounter threads_counter = new ThreadsCounter(2);
    String[] urls;
    int idx = 0;

    test_urls(String[] _urls) {urls = _urls;}
    void set_threads_counter(int value) {threads_counter.setValue(value);}
    void call(){
        if (idx == urls.length)
          {System.out.println("main thread end");}
        else if (threads_counter.value <= 0)
              {//System.out.println("full threads, sleep 5s");
                  try {
                      Thread.sleep(5000);
                  } catch (InterruptedException e) {
                      e.printStackTrace();
                  }
                  call();}
        else {threads_counter.setValue(threads_counter.value - 1);
             test_url test = new test_url(urls[idx], threads_counter);
             Thread test_thread = new Thread(test);
             test_thread.start();
             System.out.println("new threads " + urls[idx] + " sleep 5s");

             idx = idx + 1;
            try {
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            call();}
    }
}

class test_url implements Runnable{
    ThreadsCounter threads_counter;
    String url;
    WebDriver driver;
    String connected_state = null;
    int short_term = 5000;
    int normal_term = 10000;

    test_url(String _url, ThreadsCounter _threads_counter){
        url = _url;
        threads_counter = _threads_counter;
        driver = new ChromeDriver();}

    public void run() {
        try {
            //JBrowserDriver driver = new JBrowserDriver();

            //TODO account
            //login
            driver.get("http://www.websitepulse.com/"); Thread.sleep(normal_term);
            driver.findElement(By.xpath("//input[@id='usernamefield']")).sendKeys("samtso66"); Thread.sleep(short_term);
            driver.findElement(By.xpath("//input[@id='passwordfield']")).sendKeys("s654321"); Thread.sleep(short_term);
            driver.findElement(By.xpath("//input[@id='passwordfield']")).submit(); Thread.sleep(normal_term);

            //test url
            driver.get("http://www.websitepulse.com/help/testtools.china-test.html"); Thread.sleep(normal_term);
            driver.findElement(By.xpath("//input[@name='usealtlocation']")).click(); Thread.sleep(short_term); //cancel usa
            WebElement url_text_form = driver.findElement(By.xpath("//td[@class='tdcr']/input")); Thread.sleep(short_term);
            url_text_form.sendKeys(url); Thread.sleep(short_term);
            url_text_form.submit(); Thread.sleep(normal_term);

            //connected?
            (new Object() {
                WebDriver driver;

                void call(WebDriver _driver) throws InterruptedException {
                    init(_driver);
                    gain_result();
                }

                void init(WebDriver _driver) {driver = _driver;}

                void gain_result() throws InterruptedException {
                    List<WebElement> result = driver.findElements(By.xpath("//div[@id='result']/h3"));
                    if (result.size() == 0) {
                        //System.out.println("wait result sleep 5");
                        Thread.sleep(short_term);
                        gain_result();
                    } else {
                        String connected_state =
                                driver.findElement(By.xpath("//td[@class='tdcr']/table/tbody/tr[5]/td[2]/font")).getText();
                        boolean isConnected = connected_state.equals("OK");
                        try {
                            PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter("myfile.txt", true)));
                            out.println(url + ":" + isConnected);
                            out.close();
                        } catch (IOException e) {}
                        System.out.println(isConnected);
                    }
                }}
            ).call(driver);

            //logout
            driver.get("http://www.websitepulse.com/logout.php");
            Thread.sleep(normal_term);


            threads_counter.setValue(threads_counter.value + 1);
            System.out.println(url + " child thread end");
            driver.quit();
        }catch (Exception e) {
            try {driver.quit();}catch (Exception e1){}
            if (connected_state == null) {
                try {
                  PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter("myfile.txt", true)));
                  out.println(url + ":test_failed");
                  out.close();
                } catch (IOException e2) {}
                System.out.println("test_failed");
            }

            threads_counter.setValue(threads_counter.value + 1);
            System.out.println(url + " child thread end");

            e.printStackTrace();
        }

    }
}

class ThreadsCounter{
    int value;
    ThreadsCounter(int _value) {value = _value;}
    void setValue(int _value) {value = _value;}
}

