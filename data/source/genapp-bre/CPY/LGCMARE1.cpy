      ******************************************************************        
      *                                                                *        
      * LICENSED MATERIALS - PROPERTY OF IBM                           *        
      *                                                                *        
      * "RESTRICTED MATERIALS OF IBM"                                  *        
      *                                                                *        
      * CB12                                                           *        
      *                                                                *        
      * (C) COPYRIGHT IBM CORP. 2011, 2013 ALL RIGHTS RESERVED         *        
      *                                                                *        
      * US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,      *        
      * OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE                   *        
      * CONTRACT WITH IBM CORPORATION                                  *        
      *                                                                *        
      *                                                                *        
      *               COPYBOOK for COMMAREA structure                  *        
      *                                                                *        
      *   This commarea can be used for all functions                  *        
      *                                                                *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           03 CA-REQUEST-ID            PIC X(6).                                
           03 CA-RETURN-CODE           PIC 9(2).                                
           03 CA-CUSTOMER-NUM          PIC 9(10).                               
      *    Fields used in INQ All and ADD customer                              
           03 CA-CUSTOMER-REQUEST.                                              
              05 CA-FIRST-NAME         PIC X(10).                               
              05 CA-LAST-NAME          PIC X(20).                               
              05 CA-DOB                PIC X(10).                               
              05 CA-HOUSE-NAME         PIC X(20).                               
              05 CA-HOUSE-NUM          PIC X(4).                                
              05 CA-POSTCODE           PIC X(8).                                
              05 CA-NUM-POLICIES       PIC 9(3).                                
              05 CA-PHONE-MOBILE       PIC X(20).                               
              05 CA-PHONE-HOME         PIC X(20).                               
              05 CA-EMAIL-ADDRESS      PIC X(100).                              
              05 CA-POLICY-DATA        PIC X(32267).                            
