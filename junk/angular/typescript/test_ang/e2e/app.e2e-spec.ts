import { TestAngPage } from './app.po';

describe('test-ang App', function() {
  let page: TestAngPage;

  beforeEach(() => {
    page = new TestAngPage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
